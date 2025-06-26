pub type Symbol = string_interner::DefaultSymbol;
pub type SymbolTable = string_interner::StringInterner<string_interner::backend::BucketBackend>;

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, PartialEq, Clone, PartialOrd, Copy)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(Symbol),
    StringLiteral(Symbol),
    Number(u32),
    //todo
    //    Double(f32),
    And,
    Class,
    Else,
    False,
    For,
    Func,
    If,
    Nil,
    Or,
    Print,
    Return,
    Return0,
    Return1,
    Super,
    This,
    True,
    Var,
    While,
    Range,
    Error,
    Invalid,
    Discard,
    EOF,
    BitAnd,
    BitOr,
    Float(f32),
    Mod,
}

impl Token {
    pub const fn is_string_literal(&self) -> bool {
        matches!(self, Token::StringLiteral(_))
    }

    pub const fn is_number(&self) -> bool {
        matches!(self, Token::Number(_))
    }

    pub const fn is_unary_op(&self) -> bool {
        matches!(self, Token::Bang | Token::Minus)
    }

    pub const fn is_term_op(&self) -> bool {
        matches!(self, Token::Plus | Token::Minus)
    }
    pub const fn is_comparison_op(&self) -> bool {
        matches!(
            self,
            Token::GreaterEqual | Token::LessEqual | Token::Less | Token::Greater
        )
    }
    pub const fn is_equality_op(&self) -> bool {
        matches!(self, Token::EqualEqual | Token::BangEqual)
    }
    pub const fn is_factor_op(&self) -> bool {
        matches!(self, Token::Slash | Token::Star)
    }

    pub fn get_iden_symbol_unchecked(self) -> Symbol {
        if let Token::Identifier(symbol) = self {
            return symbol;
        }
        panic!("{self} is not a symbol");
    }

    //    pub fn get_iden_symbol_unchecked(&self) -> Symbol {
    //        if let Token::Identifier(symbol) = self {
    //            return *symbol;
    //        }
    //        panic!("{} is not a symbol", self);
    //    }

    pub const fn get_iden_symbol(&self) -> Option<Symbol> {
        if let Token::Identifier(symbol) = self {
            return Some(*symbol);
        }
        None
    }
    pub const fn is_binary_op(&self) -> bool {
        matches!(
            self,
            Token::Minus
                | Token::Plus
                | Token::Slash
                | Token::Star
                | Token::Greater
                | Token::GreaterEqual
                | Token::Less
                | Token::LessEqual
                | Token::And
                | Token::EqualEqual
                | Token::Or
        )
    }
}

impl From<Token> for usize {
    fn from(token: Token) -> usize {
        match token {
            Token::LeftParen => 1,
            Token::RightParen => 2,
            Token::LeftBrace => 3,
            Token::RightBrace => 4,
            Token::Comma => 5,
            Token::Dot => 6,
            Token::Minus => 7,
            Token::Plus => 8,
            Token::Semicolon => 9,
            Token::Slash => 11,
            Token::Star => 12,
            Token::Bang => 13,
            Token::BangEqual => 14,
            Token::Equal => 15,
            Token::EqualEqual => 16,
            Token::Greater => 17,
            Token::GreaterEqual => 18,
            Token::Less => 19,
            Token::LessEqual => 20,
            Token::Identifier(_) => 21,
            Token::StringLiteral(_) => 22,
            Token::Number(_) => 23,
            //            Token::Double(_) => 24,
            Token::And => 25,
            Token::Class => 26,
            Token::Else => 27,
            Token::False => 28,
            Token::For => 29,
            Token::Func => 30,
            Token::If => 31,
            Token::Nil => 32,
            Token::Or => 33,
            Token::Print => 34,
            Token::Return => 35,
            Token::Return0 => 36,
            Token::Return1 => 37,
            Token::Super => 38,
            Token::This => 39,
            Token::True => 40,
            Token::Var => 41,
            Token::While => 42,
            Token::Range => 43,
            Token::Error => 44,
            Token::Invalid => 45,
            Token::Discard => 46,
            Token::EOF => 47,
            Token::BitAnd => 48,
            Token::BitOr => 49,
            Token::Float(_) => 50,
            Token::Mod => 50,
        }
    }
}

pub struct Scanner<'char_iter> {
    pub char_iter: std::iter::Peekable<std::str::CharIndices<'char_iter>>,
    pub tokens: Vec<Token>,
    pub interner: SymbolTable,
    pub source: &'static str,
}

impl Scanner<'_> {
    pub fn new(source: &'static str) -> Self {
        let interner = SymbolTable::new();
        Self {
            source,
            char_iter: source.char_indices().peekable(),
            tokens: Vec::new(),
            interner,
        }
    }

    pub fn scan_punctuation(&mut self, pass_token: &char, pos: usize) {
        let token = match pass_token {
            '\n' => Token::Discard,
            ' ' => Token::Discard,
            '+' => Token::Plus,
            '-' => match self.char_iter.next_if(|(_pos, ch)| !ch.is_ascii_digit()) {
                Some(_minus) => Token::Minus,
                None => self.scan_digit(pos),
            },
            '=' => match self.char_iter.next_if_eq(&(pos + 1, '=')) {
                Some(_equals) => Token::EqualEqual,
                None => Token::Equal,
            },

            '<' => match self.char_iter.next_if_eq(&(pos + 1, '=')) {
                Some(_equals) => Token::LessEqual,
                None => Token::Less,
            },

            '>' => match self.char_iter.next_if_eq(&(pos + 1, '=')) {
                Some(_equals) => Token::GreaterEqual,
                None => Token::Greater,
            },

            '!' => match self.char_iter.next_if_eq(&(pos + 1, '=')) {
                Some(_equals) => Token::BangEqual,
                None => Token::Bang,
            },
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ';' => Token::Semicolon,
            '.' => match self.char_iter.next_if_eq(&(pos + 1, '.')) {
                Some(_equals) => Token::Range,
                None => Token::Dot,
            },
            '/' => Token::Slash,
            ',' => Token::Comma,
            '*' => Token::Star,
            '&' => match self.char_iter.next_if_eq(&(pos + 1, '&')) {
                Some(_equals) => Token::And,
                None => Token::BitAnd,
            },
            '|' => match self.char_iter.next_if_eq(&(pos + 1, '|')) {
                Some(_equals) => Token::Or,
                None => Token::BitOr,
            },
            '%' => Token::Mod,

            '"' => {
                let mut end = 0;
                while let Some((_pos, _)) = self.char_iter.next_if(|(_pos, ch)| *ch != '"') {
                    end = _pos;
                }
                end += 2;

                if let Some(last_char) = &self.source[pos..end].chars().last() {
                    assert_eq!(last_char, &'"');
                }

                self.char_iter.next();
                Token::StringLiteral(self.interner.get_or_intern_static(&self.source[pos..end]))
            }

            //TODO placeholder
            _ => todo!("{pass_token}"),
        };
        match token {
            Token::Discard => {}
            //            Token::Invalid => (),
            _ => self.tokens.push(token),
        }
    }

    pub fn scan_digit(&mut self, pos: usize) -> Token {
        let mut end = 0;
        while let Some((_pos, ch)) = self
            .char_iter
            .next_if(|(_pos, ch)| ch.is_numeric() || *ch == '.' || *ch == '-')
        {
            end = _pos + 1;
        }

        if pos >= end {
            // case of single digits
            end = pos + 1;
        }

        let num_string: &str = &self.source[pos..end];

        if let Ok(res) = num_string.parse::<u32>() {
            Token::Number(res)
        } else {
            Token::Float(num_string.parse::<f32>().expect("Invalid number format"))
        }
    }

    pub fn scan_func_decl() -> Token {
        todo!()
    }

    pub fn lookup_keyword(&mut self, id: &'static str) -> Token {
        match id {
            "func" => Token::Func,
            "for" => Token::For,
            "true" => Token::True,
            "false" => Token::False,
            "else" => Token::Else,
            "and" => Token::And,
            "class" => Token::Class,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "return" => Token::Return,
            "super" => Token::Super,
            "var" => Token::Var,
            "while" => Token::While,
            "print" => Token::Print,
            "ret" => Token::Return,
            _ => Token::Identifier(self.interner.get_or_intern_static(id)),
        }
    }

    pub fn scan_alphabetic(&mut self, pos: usize) {
        let mut end = 0;
        while let Some((_pos, _)) = self
            .char_iter
            .next_if(|(_pos, ch)| ch.is_alphanumeric() || *ch == '_')
        {
            end = _pos + 1;
        }

        // case of single letter
        if pos >= end {
            end = pos + 1;
        }

        let token: Token = self.lookup_keyword(&self.source[pos..end]);
        self.tokens.push(token);
    }

    pub fn scan(&mut self) {
        while let Some((pos, ch)) = self.char_iter.next() {
            match ch {
                ch if ch.is_numeric() || ch == '.' => {
                    let token = self.scan_digit(pos);
                    self.tokens.push(token);
                }
                ch if ch.is_alphabetic() || ch == '_' => self.scan_alphabetic(pos),
                ch if ch.is_ascii_punctuation() => self.scan_punctuation(&ch, pos),
                ch if ch.is_whitespace() => {}
                _ => panic!("invalid char: {ch}"),
            };

            //            if ch.is_numeric() || ch == '.' {
            //                self.scan_digit(pos)
            //            } else if ch.is_alphabetic() || ch == '_' {
            //                self.scan_alphabetic(pos)
            //            } else if ch.is_ascii_punctuation() {
            //                self.scan_punctuation(&ch, pos)
            //            }
            // TODO: invalid char
        }
        self.tokens.push(Token::EOF);
    }
}

#[cfg(test)]
mod scanner_tests {
    use super::*;

    #[test]
    fn if_statement() {
        let mut interner = SymbolTable::new();
        let test_var = interner.get_or_intern_static("test_var");
        let test_str = interner.get_or_intern_static("\"test_str\"");
        let else_var = interner.get_or_intern_static("else_var");
        let else_str = interner.get_or_intern_static("\"else_str\"");
        static SOURCE: &str = "if (10 == 11) {
                    var test_var = \"test_str\";
                    } else {
                    var else_var = \"else_str\";
                    }";
        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        assert_eq!(
            vec![
                Token::If,
                Token::LeftParen,
                Token::Number(10),
                Token::EqualEqual,
                Token::Number(11),
                Token::RightParen,
                Token::LeftBrace,
                Token::Var,
                Token::Identifier(test_var),
                Token::Equal,
                Token::StringLiteral(test_str),
                Token::Semicolon,
                Token::RightBrace,
                Token::Else,
                Token::LeftBrace,
                Token::Var,
                Token::Identifier(else_var),
                Token::Equal,
                Token::StringLiteral(else_str),
                Token::Semicolon,
                Token::RightBrace,
                Token::EOF,
            ],
            scanner.tokens
        );
    }

    #[test]
    fn var_assignment() {
        static SOURCE: &str = "var test_iden = 100 * 200.3 + -69.2;";
        let mut scanner = Scanner::new(SOURCE);
        let mut interner = SymbolTable::new();
        let test_iden = interner.get_or_intern_static("test_iden");

        scanner.scan();

        let correct_result: Vec<Token> = vec![
            Token::Var,
            Token::Identifier(test_iden),
            Token::Equal,
            Token::Number(100),
            Token::Star,
            Token::Float(200.3_f32),
            Token::Plus,
            Token::Float(-69.2),
            Token::Semicolon,
            Token::EOF,
        ];

        assert_eq!(scanner.tokens, correct_result)
    }

    #[test]
    fn fun_and_call() {
        static SOURCE: &str = r#"func test(a, b, c) {
            a - b * c;
            }
            test(1, 2, 3);"#;

        let mut interner = SymbolTable::new();
        let test = interner.get_or_intern_static("test");
        let a = interner.get_or_intern_static("a");
        let b = interner.get_or_intern_static("b");
        let c = interner.get_or_intern_static("c");

        let a = Token::Identifier(a);
        let b = Token::Identifier(b);
        let c = Token::Identifier(c);
        let name: Token = Token::Identifier(test);

        let mut scanner = Scanner::new(SOURCE);
        scanner.scan();

        let expected_result: Vec<Token> = vec![
            Token::Func,
            name,
            Token::LeftParen,
            a,
            Token::Comma,
            b,
            Token::Comma,
            c,
            Token::RightParen,
            Token::LeftBrace,
            a,
            Token::Minus,
            b,
            Token::Star,
            c,
            Token::Semicolon,
            Token::RightBrace,
            name,
            Token::LeftParen,
            Token::Number(1),
            Token::Comma,
            Token::Number(2),
            Token::Comma,
            Token::Number(3),
            Token::RightParen,
            Token::Semicolon,
            Token::EOF,
        ];

        assert_eq!(scanner.tokens, expected_result)
    }
}
