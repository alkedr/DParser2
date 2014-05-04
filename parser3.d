import std.stdio;
import std.typecons;
import std.algorithm;
import std.conv;
import std.string;
import std.array;


/*
	new representation: sequence of choices of sequences of choices of ...
	sequences can be optional

	sequence:
		if (cond1) {
			...
		} else {
			error
		}
		if (cond2) {
			...
		} else {
			error
		}

	optional sequence:
		if (cond1) {
			...
		}
		if (cond2) {
			...
		}

	choice:
		if (cond1) {
			...
		} else {
			if (cond2) {
				...
			} else {
				error
			}
		}
*/

/*
class ParserGenerator2 {
	string condition;





	class Rule {
		string condition;
		ParserGenerator2 thenBranch;
	}

	Rule[] rules;



	void addToSequence(ParserGenerator2 pg) {
		if (rules.length > 0) {
			foreach (rule; rules) {
				rule.thenBranch.addToSequence(pg);
			}
		} else {
			addToChoice(pg);
		}
	}

	void addToChoice(ParserGenerator2 pg) {
		rules ~= pg.rules;
	}


	string code(string[] tempVarNumberToFieldMap) {

	}





	static ParserGenerator2 sequence(ParserGenerator2... pgs) {
		ParserGenerator2 result = new ParserGenerator2;
		foreach (pg; pgs) {
			result.addToSequence(pg);
		}
		return result;
	}

	static ParserGenerator2 choice(ParserGenerator2... pgs) {
		ParserGenerator2 result = new ParserGenerator2;
		foreach (pg; pgs) {
			result.addToChoice(pg);
		}
		return result;
	}


}
*/








/*

	auto beginN = startToken();
	if (starting symbol is correct) {
		finish parsing
		auto endN = endToken()
		next item in sequence if previous wasn't optional
	} else {
		next item in choice
	}
	next item in sequence if previous was optional

*/



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



class TokenParsingRule {
	string condition;
	string before;
	string after;
	string nodeType;
	string fieldName;
	TokenParsingRule nextInSequence;
	TokenParsingRule nextInChoice;
	TokenParsingRule nextInSequenceOfOptionals;
	bool startingNewToken;
	bool isList;

	TokenParsingRule sequenceEnd() {
		auto result = this;
		while (result.nextInSequence !is null) result = result.nextInSequence;
		return result;
	}

	TokenParsingRule choiceEnd() {
		auto result = this;
		while (result.nextInChoice !is null) result = result.nextInChoice;
		return result;
	}

	TokenParsingRule sequenceOfOptionalsEnd() {
		auto result = this;
		while (result.nextInSequenceOfOptionals !is null) result = result.nextInSequenceOfOptionals;
		return result;
	}


	void addToSequence(TokenParsingRule rule) {
		sequenceEnd.nextInSequence = rule;
		//if (nextInSequence is null) {
		//	nextInSequence = rule;
		//} else {
		//	writeln(nextInSequence.condition, " == ", rule.condition);
		//	if (nextInSequence.condition == rule.condition) {
		//		nextInSequence.addToChoice(rule.nextInSequence);
		//	} else {
		//		nextInSequence.addToSequence(rule);
		//	}
		//}
	}
	void addToChoice(TokenParsingRule rule) {
		if (nextInChoice is null)
			nextInChoice = rule;
		else
			nextInChoice.addToChoice(rule);
	}
	void addToSequenceOfOptionals(TokenParsingRule rule) {
		if (nextInSequenceOfOptionals is null)
			nextInSequenceOfOptionals = rule;
		else
			nextInSequenceOfOptionals.addToSequenceOfOptionals(rule);
	}

	string storingCode(string[] structTypes, string[] structFieldNamesForTempVariables) {
		string result;
		foreach (number, type; structTypes) {
			result ~= format("%s s%d=new %s;", type, number, type);
		}
		foreach (number, name; structFieldNamesForTempVariables) {
			result ~= format("%s=t%d;", name, number);
		}
		return result;
	}

	string code(
		int depth = 0,
		string currentStructVariableName = "",
		string[] structTypes = [],
		string[] structFieldNamesForTempVariables = [])
	{
		string result;

		if (startingNewToken) result ~= "skipCrap;";
		//result ~= before;
		result ~= "if(" ~ condition ~ "){";
		if (startingNewToken) result ~= "auto t" ~ to!string(depth) ~ "=c++;";
		if (nextInSequence is null) {
			result ~= storingCode(structTypes, structFieldNamesForTempVariables);
		} else {
			result ~= nextInSequence.code(
				depth + startingNewToken,
				fieldName.empty ? currentStructVariableName : currentStructVariableName ~ "." ~ fieldName,
				nodeType.empty ? structTypes : structTypes ~ nodeType,
				fieldName.empty ? structFieldNamesForTempVariables : structFieldNamesForTempVariables ~ (currentStructVariableName ~ fieldName)
			);
		}
		result ~= "}else{";
		if (nextInChoice is null) {
			result ~= "error(noMatch);";
		} else {
			result ~= nextInChoice.code;
		}
		result ~= "}";
		//result ~= after;
		if (nextInSequenceOfOptionals !is null) {
			result ~= nextInSequenceOfOptionals.code;
		}

		return result;

		//return
		//	(startingNewToken ? "skipCrap;" : "") ~
		//	before ~
		//	"if(" ~ condition ~ "){" ~
		//		(startingNewToken ? "auto token" ~ to!string(depth) ~ "Begin = c;" : "") ~
		//		"c++;" ~
		//		((nextInSequence is null)
		//			? storingCode(structTypes, structFieldNamesForTempVariables)
		//			: nextInSequence.code(depth + startingNewToken, structTypes, structFieldNamesForTempVariables)) ~
		//	"}else{" ~
		//		((nextInChoice is null) ? "error(noMatch);" : nextInChoice.code) ~
		//	"}" ~
		//	after ~
		//	nextInSequenceOfOptionals.code;
	}

	static TokenParsingRule field(string fieldName)(TokenParsingRule contents) {
		contents.fieldName = fieldName;
		return contents;
	}

	static TokenParsingRule list(string fieldName, string separator)(TokenParsingRule item) {
		return null;
	}

	static TokenParsingRule dotList(string fieldName)(TokenParsingRule item) {
		return list!(fieldName, ".")(item);
	}

	static TokenParsingRule commaList(string fieldName)(TokenParsingRule item) {
		return list!(fieldName, ",")(item);
	}

	static TokenParsingRule identifier() {
		return null;
	}

	static TokenParsingRule keyword(dstring s)() {
		static if (s.length > 0) {
			return ifCharThen(s[0], keyword!(s[1..$]));
		} else {
			return ifThen("!isIdentifierChar(text[c])");
		}
	}

	private static TokenParsingRule ifCharThen(dchar c, TokenParsingRule nextInSequence) {
		return ifThen(format("text[c]=='%c'", c), nextInSequence);  //\\U%08d
	}

	private static TokenParsingRule ifThen(string condition, TokenParsingRule nextInSequence = null) {
		TokenParsingRule result = new TokenParsingRule;
		result.condition = condition;
		result.nextInSequence = nextInSequence;
		return result;
	}
}






struct ParserGenerator {

	private struct Rule {
		string condition;
		ParserGenerator thenBranch;
		ParserGenerator elseBranch;

		string toString() const {
			return
				// (startingNewToken ? "skipCrap();" : "") ~
				"if(" ~ condition ~ "){" ~
					//"auto tokenXBegin = c;" ~
					"c++;" ~
					thenBranch.code ~
				"}else{" ~
					(elseBranch.code.empty ? "error(noMatch);" : elseBranch.code) ~
				"}";
		}
	}

	private string action;
	private Rule[] rules;
	private string[][string] variables;


	public string code() const {
		// 1. finish parsing this token
		// 2. rules
		// 3. storing

		// before - parse lists, identifiers etc
		// after - store this token if necessary
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
			return ifThen("!isIdentifierChar(text[c])", ParserGenerator());
		}
	}

	private static ParserGenerator ifCharThen(dchar c, ParserGenerator thenBranch) {
		return ifThen(format("text[c]=='%c'", c), thenBranch);  //\\U%08d
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

	auto pg = ParserGenerator.keyword!"a";
	pg.sequence(ParserGenerator.keyword!"b");
	writeln(pg.code);

	//auto pg = ParserGenerator.keyword!"a";
	//pg.choice(ParserGenerator.keyword!"b");
	//writeln(pg.code);

	//auto pg = ParserGenerator.keyword!"a1";
	//pg.choice(ParserGenerator.keyword!"a2");
	//pg.sequence(ParserGenerator.keyword!"yyy");
	//writeln(pg.code);

	//auto pg = ParserGenerator.keyword!"a1";
	//pg.sequence(ParserGenerator.keyword!"a2");
	//writeln(pg.code);
}


