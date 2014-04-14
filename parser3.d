import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;

/*
Sequence:
	if (cond1) {
		action1
		if (cond2) {
			action2
		}
	} - при таком способе будет дублироваться код в случае sequence(choice(A, B, C), X) [будет 3 икса]
	    хорошо для ключевых слов

	or

	if (cond1) {
		action1
	} else {
		error()
	}

	if (cond2) {
		action2
	} else {
		error()
	} - при таком способе будет дублироваться код обработки ошибок
	    (на каждую букву ключевого слова отдельный error())
	    плохо для ключевых слов, хорошо для всего остального


Choice:
	if (cond1) {
		action1
	} else {
		if (cond2) {
			action2
		}
	}

Optional:
	if (cond1) {
		action1
	}
	if (cond2) {
		action2
	}
*/
// Храним информацию о полях и временных переменных в листах
// После построения дерева пытаемся протолкнуть их вверх
struct ParserGenerator {

	private struct Rule {
		string condition;
		ParserGenerator thenBranch;
		ParserGenerator elseBranch;

		string toString() const {
			return "if(" ~ condition ~ "){" ~ thenBranch.code ~ "}else{" ~ elseBranch.code ~ "}";
		}
	}

	private string action;
	private Rule[] rules;
	private string[][string] variables;


	public string code() const {
		return action ~ join(map!"a.toString"(rules));
	}


	// merges item into every leaf of this
	// TODO: rules ~= item, error if doesn't match (replace empty else's with errors?)
	public void sequence(ParserGenerator item) {
		if (rules.length == 0) {
			choice(item);
		} else {
			foreach (rule; rules) {
				rule.thenBranch.sequence(item);
			}
		}
	}

	// merges item into this
	public void choice(ParserGenerator item) {
		action ~= item.action;
		foreach (key, value; item.variables) {
			variables[key] = value;
		}
		foreach (newRule; item.rules) {
			if (rules.length == 0) {
				rules ~= newRule;
			} else {
				auto foundRules = find!"a.condition == b.condition"(rules, newRule);
				if (foundRules.length == 0) {
					rules[$-1].elseBranch.choice(item);
				} else {
					assert(foundRules.length == 1);
					foundRules[0].thenBranch.choice(newRule.thenBranch);
					foundRules[0].elseBranch.choice(newRule.elseBranch);
				}
			}
		}
	}

	//rules ~= item, skip if doesn't match
	public void optional(ParserGenerator contents) {
		assert(contents.rules.length == 1);
		rules ~= contents.rules;
	}





	//private static ParserGenerator action(string s) {
	//	ParserGenerator result = ParserGenerator();
	//	result.action = s;
	//	return result;
	//}

	static ParserGenerator field(string fieldName)(ParserGenerator contents) {
		contents.setStorageName(fieldName ~ ".");
		return contents;
	}

	static ParserGenerator list(string fieldName, string separator)(ParserGenerator item) {
		return null;
	}

	static ParserGenerator dotList(string fieldName)(ParserGenerator item) {
		return list!(fieldName, ".")(item);
	}

	static ParserGenerator commaList(string fieldName)(ParserGenerator item) {
		return list!(fieldName, ",")(item);
	}

	static ParserGenerator identifier() {
		return ParserGenerator();
	}

	static ParserGenerator keyword(dstring s)() {
		static if (s.length > 0) {
			return ifCharThen(s[0], keyword!(s[1..$]));
		} else {
			return ifThen("!isIdentifierChar(currentChar)", ParserGenerator());
		}
	}

	private static ParserGenerator ifCharThen(dchar c, ParserGenerator thenBranch) {
		return ifThen(format("currentChar=='%c'", c), thenBranch);  //\\U%08d
	}

	private static ParserGenerator ifThen(string condition, ParserGenerator thenBranch) {
		ParserGenerator result = ParserGenerator();
		Rule newRule;
		newRule.condition = condition;
		newRule.thenBranch = thenBranch;
		result.rules ~= newRule;
		return result;
	}
}




unittest {
	//writeln(ParserGenerator.keyword!"import".code);

	//auto pg = ParserGenerator.keyword!"a";
	//pg.sequence(ParserGenerator.keyword!"b");
	//writeln(pg.code);

	//auto pg = ParserGenerator.keyword!"a";
	//pg.choice(ParserGenerator.keyword!"b");
	//writeln(pg.code);

	auto pg = ParserGenerator.keyword!"a1";
	pg.choice(ParserGenerator.keyword!"a2");
	writeln(pg.code);

	//auto pg = ParserGenerator.keyword!"a1";
	//pg.sequence(ParserGenerator.keyword!"a2");
	//writeln(pg.code);
}


