/* ============================================================
   CALX-9X  |  Scientific Calculator — Full Engine
   Recursive descent parser: no eval(), proper precedence,
   full scientific function support, robust state machine.
   ============================================================ */

(() => {
  'use strict';

  /* ===== CONFIG ===== */
  const MAX_DISPLAY = 15;   // significant digits shown

  /* ===== STATE ===== */
  let tokens    = [];        // array of token strings building the expression
  let memory    = 0;
  let isDeg     = true;
  let answered  = false;     // did we just press =?
  let waitingPowY = false;   // did user press xʸ and we await the exponent?
  let powYBase  = null;

  /* ===== DOM ===== */
  const exprEl   = document.getElementById('exprLine');
  const resultEl = document.getElementById('resultLine');
  const angBadge = document.getElementById('angleBadge');
  const memBadge = document.getElementById('memBadge');

  /* ===================================================================
     TOKENIZER  →  takes raw token array and makes a display string
  =================================================================== */
  function tokensToExpr(toks) {
    return toks.join('');
  }

  /* ===================================================================
     RECURSIVE DESCENT PARSER
     Grammar:
       expr     = additive
       additive = multiplicative (('+' | '-') multiplicative)*
       multiplicative = unary (('*' | '/' | '%') unary)*
       unary    = '-' unary | power
       power    = primary ('^' unary)*
       primary  = number | '(' expr ')' | func '(' expr ')'
  =================================================================== */

  /* ----- Lexer ----- */
  const TOK = {
    NUM: 'NUM', PLUS: '+', MINUS: '-', MUL: '*', DIV: '/', MOD: '%',
    POW: '^', LPAREN: '(', RPAREN: ')', FUNC: 'FUNC', EOF: 'EOF'
  };

  function tokenize(str) {
    const tokens = [];
    let i = 0;
    const s = str.trim();
    while (i < s.length) {
      const c = s[i];
      // whitespace
      if (/\s/.test(c)) { i++; continue; }
      // number (including scientific notation)
      if (/[\d.]/.test(c)) {
        let num = '';
        while (i < s.length && /[\d.]/.test(s[i])) num += s[i++];
        if (i < s.length && (s[i] === 'e' || s[i] === 'E')) {
          num += s[i++];
          if (i < s.length && (s[i] === '+' || s[i] === '-')) num += s[i++];
          while (i < s.length && /\d/.test(s[i])) num += s[i++];
        }
        tokens.push({ type: TOK.NUM, value: parseFloat(num) });
        continue;
      }
      // functions / constants
      const funcs = ['asin','acos','atan','sin','cos','tan','log2','log','ln',
                     'exp','sqrt','cbrt','abs','fact','mod'];
      let matched = false;
      for (const fn of funcs) {
        if (s.startsWith(fn, i)) {
          tokens.push({ type: TOK.FUNC, value: fn });
          i += fn.length;
          matched = true; break;
        }
      }
      if (matched) continue;
      // operators
      if (c === '+') { tokens.push({ type: TOK.PLUS }); i++; continue; }
      if (c === '-' || c === '−') { tokens.push({ type: TOK.MINUS }); i++; continue; }
      if (c === '*' || c === '×') { tokens.push({ type: TOK.MUL }); i++; continue; }
      if (c === '/' || c === '÷') { tokens.push({ type: TOK.DIV }); i++; continue; }
      if (c === '^') { tokens.push({ type: TOK.POW }); i++; continue; }
      if (c === '(') { tokens.push({ type: TOK.LPAREN }); i++; continue; }
      if (c === ')') { tokens.push({ type: TOK.RPAREN }); i++; continue; }
      if (c === '%') { tokens.push({ type: TOK.MOD }); i++; continue; }
      throw new Error(`Unknown char: ${c}`);
    }
    tokens.push({ type: TOK.EOF });
    return tokens;
  }

  /* ----- Parser class ----- */
  class Parser {
    constructor(lexTokens) {
      this.toks = lexTokens;
      this.pos  = 0;
    }
    peek() { return this.toks[this.pos]; }
    consume(type) {
      const t = this.toks[this.pos];
      if (type && t.type !== type) throw new Error(`Expected ${type} got ${t.type}`);
      this.pos++;
      return t;
    }
    match(...types) {
      return types.includes(this.peek().type);
    }

    parse() {
      const val = this.parseAdditive();
      if (this.peek().type !== TOK.EOF) throw new Error('Unexpected token');
      return val;
    }

    parseAdditive() {
      let left = this.parseMultiplicative();
      while (this.match(TOK.PLUS, TOK.MINUS)) {
        const op = this.consume().type;
        const right = this.parseMultiplicative();
        left = op === TOK.PLUS ? left + right : left - right;
      }
      return left;
    }

    parseMultiplicative() {
      let left = this.parseUnary();
      while (this.match(TOK.MUL, TOK.DIV, TOK.MOD)) {
        const op = this.consume().type;
        const right = this.parseUnary();
        if (op === TOK.MUL) left = left * right;
        else if (op === TOK.DIV) { if (right === 0) throw new Error('Div/0'); left = left / right; }
        else left = left % right;
      }
      return left;
    }

    parseUnary() {
      if (this.match(TOK.MINUS)) {
        this.consume();
        return -this.parseUnary();
      }
      if (this.match(TOK.PLUS)) {
        this.consume();
        return this.parseUnary();
      }
      return this.parsePower();
    }

    parsePower() {
      let base = this.parsePrimary();
      if (this.match(TOK.POW)) {
        this.consume();
        const exp = this.parseUnary();   // right-associative
        base = Math.pow(base, exp);
      }
      return base;
    }

    parsePrimary() {
      const t = this.peek();

      // number
      if (t.type === TOK.NUM) {
        this.consume();
        return t.value;
      }

      // parenthesised expression
      if (t.type === TOK.LPAREN) {
        this.consume(TOK.LPAREN);
        const val = this.parseAdditive();
        this.consume(TOK.RPAREN);
        return val;
      }

      // function call
      if (t.type === TOK.FUNC) {
        this.consume();
        this.consume(TOK.LPAREN);
        const arg = this.parseAdditive();
        this.consume(TOK.RPAREN);
        return applyFunction(t.value, arg);
      }

      throw new Error(`Unexpected: ${t.type}`);
    }
  }

  /* ----- Math function evaluator ----- */
  function toRad(deg) { return deg * Math.PI / 180; }
  function toDeg(rad) { return rad * 180 / Math.PI; }

  function applyFunction(fn, arg) {
    const a = isDeg ? toRad(arg) : arg;   // angle param for trig
    switch (fn) {
      case 'sin':  return round15(Math.sin(a));
      case 'cos':  return round15(Math.cos(a));
      case 'tan': {
        // catch asymptotes
        const t = Math.tan(a);
        if (Math.abs(t) > 1e14) throw new Error('Undefined');
        return round15(t);
      }
      case 'asin': {
        if (arg < -1 || arg > 1) throw new Error('Domain');
        const r = Math.asin(arg);
        return isDeg ? round15(toDeg(r)) : round15(r);
      }
      case 'acos': {
        if (arg < -1 || arg > 1) throw new Error('Domain');
        const r = Math.acos(arg);
        return isDeg ? round15(toDeg(r)) : round15(r);
      }
      case 'atan': {
        const r = Math.atan(arg);
        return isDeg ? round15(toDeg(r)) : round15(r);
      }
      case 'log':  if (arg <= 0) throw new Error('Domain'); return round15(Math.log10(arg));
      case 'ln':   if (arg <= 0) throw new Error('Domain'); return round15(Math.log(arg));
      case 'log2': if (arg <= 0) throw new Error('Domain'); return round15(Math.log2(arg));
      case 'exp':  return round15(Math.exp(arg));
      case 'sqrt': if (arg < 0) throw new Error('Domain'); return round15(Math.sqrt(arg));
      case 'cbrt': return round15(Math.cbrt(arg));
      case 'abs':  return Math.abs(arg);
      case 'fact': {
        const n = Math.round(arg);
        if (n < 0 || n > 170) throw new Error('Domain');
        let r = 1;
        for (let i = 2; i <= n; i++) r *= i;
        return r;
      }
      default: throw new Error('Unknown fn: ' + fn);
    }
  }

  /* remove floating-point noise: 1.9999999999999998 → 2 */
  function round15(v) { return parseFloat(v.toPrecision(14)); }

  /* ----- Evaluate expression string ----- */
  function evaluate(exprStr) {
    const lexed = tokenize(exprStr);
    const parser = new Parser(lexed);
    const result = parser.parse();
    if (!isFinite(result)) throw new Error(isNaN(result) ? 'Undefined' : 'Overflow');
    return result;
  }

  /* ===================================================================
     DISPLAY HELPERS
  =================================================================== */
  function formatNumber(n) {
    if (typeof n !== 'number') return String(n);
    if (!isFinite(n)) return isNaN(n) ? 'Undefined' : (n > 0 ? '∞' : '-∞');

    // avoid 0.30000000000000004 etc.
    const rounded = parseFloat(n.toPrecision(12));

    // choose notation
    const abs = Math.abs(rounded);
    if (abs !== 0 && (abs >= 1e15 || abs < 1e-9)) {
      // scientific notation, clean up
      return rounded.toExponential(8).replace(/\.?0+(e)/, '$1');
    }

    let str = rounded.toString();
    return str;
  }

  function setResultSize(str) {
    resultEl.classList.remove('sz-md', 'sz-sm', 'sz-xs', 'error');
    const l = str.length;
    if (l > 20) resultEl.classList.add('sz-xs');
    else if (l > 15) resultEl.classList.add('sz-sm');
    else if (l > 10) resultEl.classList.add('sz-md');
  }

  function showResult(val) {
    const s = formatNumber(val);
    setResultSize(s);
    resultEl.textContent = s;
  }

  function showError(msg) {
    resultEl.classList.remove('sz-md','sz-sm','sz-xs');
    resultEl.classList.add('error');
    resultEl.textContent = msg;
    resultEl.classList.add('shake');
    setTimeout(() => resultEl.classList.remove('shake'), 400);
    tokens = [];
    answered = false;
  }

  function refreshDisplay() {
    exprEl.textContent = tokensToExpr(tokens) || '\u00a0';
    angBadge.textContent = isDeg ? 'DEG' : 'RAD';
    memBadge.textContent = memory !== 0 ? 'M' : '';
  }

  /* ===================================================================
     LIVE EVALUATION  — show result as user types
  =================================================================== */
  function tryLiveEval() {
    const expr = buildEvalString();
    if (!expr) { resultEl.classList.remove('sz-md','sz-sm','sz-xs','error'); resultEl.textContent = '0'; return; }
    try {
      const val = evaluate(expr);
      showResult(val);
    } catch(e) {
      // incomplete expression — just leave display as-is
    }
  }

  /* convert display tokens to evaluable string */
  function buildEvalString() {
    return tokens.join('');
  }

  /* ===================================================================
     LAST NUMBER in token stream
  =================================================================== */
  function getLastNumber() {
    // collect trailing digit/dot tokens
    let s = '';
    for (let i = tokens.length - 1; i >= 0; i--) {
      const t = tokens[i];
      if (/^[\d.]+$/.test(t) || t === 'π' || t === 'e') {
        s = t + s;
      } else break;
    }
    if (s === 'π') return Math.PI;
    if (s === 'e') return Math.E;
    return s ? parseFloat(s) : null;
  }

  function replaceLastNumber(newVal) {
    // remove trailing number tokens
    while (tokens.length > 0) {
      const t = tokens[tokens.length - 1];
      if (/^[\d.]+$/.test(t) || t === 'π' || t === 'e' || t === '-') {
        tokens.pop();
      } else break;
    }
    tokens.push(String(newVal));
  }

  /* ===================================================================
     BUTTON HANDLERS
  =================================================================== */
  function handleBtn(fn) {

    /* ---- numbers ---- */
    if (fn.startsWith('n') && fn.length === 2) {
      const digit = fn[1];
      if (answered) { tokens = []; answered = false; }
      tokens.push(digit);
      refreshDisplay();
      tryLiveEval();
      return;
    }

    switch (fn) {

      /* ---- digit-like ---- */
      case 'dot': {
        if (answered) { tokens = ['0']; answered = false; }
        // check if current number already has a dot
        let numStr = '';
        for (let i = tokens.length - 1; i >= 0; i--) {
          if (/^\d$/.test(tokens[i])) numStr = tokens[i] + numStr;
          else if (tokens[i] === '.') { numStr = '.' + numStr; }
          else break;
        }
        if (!numStr.includes('.')) {
          if (tokens.length === 0 || !/^\d$/.test(tokens[tokens.length-1])) tokens.push('0');
          tokens.push('.');
        }
        refreshDisplay();
        tryLiveEval();
        break;
      }

      /* ---- basic operators ---- */
      case 'add': appendOp('+'); break;
      case 'sub': appendOp('-'); break;
      case 'mul': appendOp('×'); break;
      case 'div': appendOp('÷'); break;
      case 'mod': appendOp('%'); break;

      /* ---- power of Y ---- */
      case 'powY': {
        // treat like an operator
        if (answered) { tokens = [resultEl.textContent]; answered = false; }
        if (tokens.length === 0) break;
        // remove trailing operator if any
        removeTrailingOp();
        tokens.push('^');
        refreshDisplay();
        answered = false;
        break;
      }

      /* ---- parentheses ---- */
      case 'lp':
        if (answered) { tokens = []; answered = false; }
        tokens.push('(');
        refreshDisplay();
        break;
      case 'rp':
        tokens.push(')');
        refreshDisplay();
        tryLiveEval();
        break;

      /* ---- clear ---- */
      case 'ac':
        tokens = [];
        answered = false;
        resultEl.classList.remove('sz-md','sz-sm','sz-xs','error');
        resultEl.textContent = '0';
        refreshDisplay();
        break;

      case 'ce':
        if (answered) { tokens = []; answered = false; resultEl.textContent = '0'; refreshDisplay(); break; }
        tokens.pop();
        refreshDisplay();
        tryLiveEval();
        break;

      case 'bsp':
        if (answered) break;
        if (tokens.length > 0) {
          const last = tokens[tokens.length - 1];
          if (last.length > 1 && /^\d+$/.test(last)) {
            tokens[tokens.length - 1] = last.slice(0, -1);
            if (tokens[tokens.length - 1] === '') tokens.pop();
          } else {
            tokens.pop();
          }
        }
        refreshDisplay();
        tryLiveEval();
        break;

      /* ---- equals ---- */
      case 'eq': {
        if (tokens.length === 0) break;
        const expr = buildEvalString();
        try {
          const val = evaluate(expr);
          exprEl.textContent = tokensToExpr(tokens) + ' =';
          tokens = [formatNumber(val)];
          showResult(val);
          answered = true;
        } catch(e) {
          exprEl.textContent = tokensToExpr(tokens);
          showError(friendlyError(e.message));
        }
        break;
      }

      /* ---- percent ---- */
      case 'pct': {
        const n = getLastNumber();
        if (n === null) break;
        replaceLastNumber(n / 100);
        refreshDisplay();
        tryLiveEval();
        break;
      }

      /* ---- negate ---- */
      case 'neg': {
        const n = getLastNumber();
        if (n === null) break;
        replaceLastNumber(-n);
        refreshDisplay();
        tryLiveEval();
        break;
      }

      /* ---- constants ---- */
      case 'pi': {
        if (answered) { tokens = []; answered = false; }
        tokens.push(String(Math.PI));
        refreshDisplay();
        tryLiveEval();
        break;
      }
      case 'eul': {
        if (answered) { tokens = []; answered = false; }
        tokens.push(String(Math.E));
        refreshDisplay();
        tryLiveEval();
        break;
      }

      /* ---- unary sci functions ---- */
      case 'sin':  applyUnary('sin');  break;
      case 'cos':  applyUnary('cos');  break;
      case 'tan':  applyUnary('tan');  break;
      case 'asin': applyUnary('asin'); break;
      case 'acos': applyUnary('acos'); break;
      case 'atan': applyUnary('atan'); break;
      case 'log':  applyUnary('log');  break;
      case 'ln':   applyUnary('ln');   break;
      case 'log2': applyUnary('log2'); break;
      case 'exp':  applyUnary('exp');  break;
      case 'sqrt': applyUnary('sqrt'); break;
      case 'cbrt': applyUnary('cbrt'); break;
      case 'abs':  applyUnary('abs');  break;
      case 'fact': applyUnary('fact'); break;
      case 'inv': {
        const n = getCurrentValue();
        if (n === null) break;
        if (n === 0) { showError('Div/0'); break; }
        const r = round15(1 / n);
        tokens = [formatNumber(r)];
        showResult(r);
        answered = true;
        exprEl.textContent = `1/(${formatNumber(n)})`;
        break;
      }
      case 'pow2': {
        const n = getCurrentValue();
        if (n === null) break;
        const r = round15(n * n);
        exprEl.textContent = `(${formatNumber(n)})²`;
        tokens = [formatNumber(r)];
        showResult(r);
        answered = true;
        break;
      }
      case 'pow3': {
        const n = getCurrentValue();
        if (n === null) break;
        const r = round15(n * n * n);
        exprEl.textContent = `(${formatNumber(n)})³`;
        tokens = [formatNumber(r)];
        showResult(r);
        answered = true;
        break;
      }

      /* ---- deg/rad ---- */
      case 'degRad':
        isDeg = !isDeg;
        refreshDisplay();
        break;

      /* ---- memory ---- */
      case 'mc':
        memory = 0;
        refreshDisplay();
        break;
      case 'mr': {
        if (answered) { tokens = []; answered = false; }
        tokens.push(formatNumber(memory));
        refreshDisplay();
        tryLiveEval();
        break;
      }
      case 'ms': {
        const v = getCurrentValue();
        if (v !== null) { memory = v; refreshDisplay(); }
        break;
      }
      case 'mplus': {
        const v = getCurrentValue();
        if (v !== null) { memory += v; refreshDisplay(); }
        break;
      }
    }
  }

  /* ---- helpers ---- */
  function appendOp(op) {
    if (answered) { answered = false; }
    if (tokens.length === 0) {
      if (op === '-') { tokens.push('-'); refreshDisplay(); return; }
      return;
    }
    removeTrailingOp();
    tokens.push(op);
    refreshDisplay();
  }

  function removeTrailingOp() {
    const ops = ['+','-','×','÷','%','^'];
    while (tokens.length > 0 && ops.includes(tokens[tokens.length-1])) tokens.pop();
  }

  function getCurrentValue() {
    if (tokens.length === 0) return null;
    const expr = buildEvalString();
    try { return evaluate(expr); }
    catch { return getLastNumber(); }
  }

  function applyUnary(fn) {
    const n = getCurrentValue();
    if (n === null) return;
    try {
      const r = applyFunction(fn, n);
      const label = {
        sin:'sin',cos:'cos',tan:'tan',
        asin:'asin',acos:'acos',atan:'atan',
        log:'log',ln:'ln',log2:'log₂',exp:'eˣ',
        sqrt:'√',cbrt:'∛',abs:'|·|',fact:'n!'
      }[fn] || fn;
      exprEl.textContent = `${label}(${formatNumber(n)})`;
      tokens = [formatNumber(r)];
      showResult(r);
      answered = true;
    } catch(e) {
      showError(friendlyError(e.message));
    }
  }

  function friendlyError(msg) {
    if (msg.includes('Div') || msg.includes('0')) return 'Div by 0';
    if (msg.includes('Domain')) return 'Domain Err';
    if (msg.includes('Overflow')) return 'Overflow';
    if (msg.includes('Undefined')) return 'Undefined';
    return 'Syntax Err';
  }

  /* ===================================================================
     RIPPLE EFFECT
  =================================================================== */
  function addRipple(btn, e) {
    const el = document.createElement('span');
    el.className = 'ripple-el';
    const r = btn.getBoundingClientRect();
    const size = Math.max(r.width, r.height) * 1.5;
    el.style.cssText = `width:${size}px;height:${size}px;` +
      `left:${e.clientX - r.left - size/2}px;top:${e.clientY - r.top - size/2}px`;
    btn.appendChild(el);
    setTimeout(() => el.remove(), 520);
  }

  /* ===================================================================
     EVENT BINDING
  =================================================================== */
  document.querySelectorAll('.btn').forEach(btn => {
    btn.addEventListener('pointerdown', e => {
      addRipple(btn, e);
    });
    btn.addEventListener('click', () => {
      handleBtn(btn.dataset.fn);
    });
  });

  /* ---- keyboard ---- */
  const keyMap = {
    '0':'n0','1':'n1','2':'n2','3':'n3','4':'n4',
    '5':'n5','6':'n6','7':'n7','8':'n8','9':'n9',
    '+':'add','-':'sub','*':'mul','/':'div',
    'Enter':'eq','=':'eq',
    'Backspace':'bsp','Delete':'ac',
    '.':'dot','%':'pct',
    '(':'lp',')':'rp',
    '^':'powY',
  };

  document.addEventListener('keydown', e => {
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    const fn = keyMap[e.key];
    if (fn) {
      e.preventDefault();
      handleBtn(fn);
      // highlight
      const btn = document.querySelector(`[data-fn="${fn}"]`);
      if (btn) {
        btn.style.transform = 'scale(0.89)';
        setTimeout(() => btn.style.transform = '', 120);
      }
    }
  });

  /* ===================================================================
     INIT
  =================================================================== */
  refreshDisplay();

})();