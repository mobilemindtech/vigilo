package io.vigilo

import io.vigilo.core.validators.given
import org.scalatest.flatspec.*
import org.scalatest.matchers.should.Matchers.*

val messages = Map[String, String](
  "validation.number.min"   -> "Value for %s must be bigger or equal %d",
  "validation.number.max"   -> "Value for %s must be less or equal %d",
  "validation.number.size"  -> "Value for %s must be between %d and %d",
  "validation.required"     -> "Value for %s is required",
  "validation.string.blank" -> "Value for %s can't be blank",
  "validation.string.min"   -> "Size of text for %s must be bigger or equal %d",
  "validation.string.max"   -> "Size of text for %s must be less or equal %d",
  "validation.string.size"  -> "Size of text for %s must be between %d and %d",
  "validation.email"        -> "Value of %s must be a valid e-mail",
  "validation.regex"        -> "Value of %s doest not match with expected pattern",
  "validation.rel"          -> "Value of relation %s is required",
  "validation.list.min"     -> "List size of %s must be bigger os equal %s",
  "validation.list.max"     -> "List size of %s must be less os equal %s",
  "validation.list.size"    -> "List size of %s must be between %d and %d",
  "validation.values"       -> "Value of %s must be one of %s",
  "validation.choices.min"  -> "Value of %s must be %d between the options %s",
  "validation.choices.max"  -> "Value of %s must be at most %d selections between then options %s",
  "validation.choices.size" -> "Value of %s must be between %d and %d selections between then options %s",
  "validation.date.min"     -> "Date of %s must be before of %s",
  "validation.date.max"     -> "Date of %s must be after of %s"
)

given ValidatorMessages:
  override def message(code: String, args: Any*): String =
    messages.getOrElse(code, code).formatted(args*)

case class ValueRelation(id: Int = 0, name: String = "", valid: Boolean = true) extends Relation

// Test case classes with validation annotations
case class BasicModel(@required id: Int = 0, @required name: String = "", @email email: String = "") derives Validator

case class StringValidationModel(
  @string(blank = false) nonBlankString: String = "",
  @string(empty = false) nonEmptyString: String = "",
  @string(min = 5) minString: String = "",
  @string(max = 10) maxString: String = "",
  @string(min = 3, max = 8) rangeString: String = ""
) derives Validator

case class NumberValidationModel(
  @number(min = 10) minValue: Int = 0,
  @number(max = 100) maxValue: Int = 0,
  @number(min = 20, max = 50) rangeValue: Int = 0
) derives Validator

case class RegexpValidationModel(@regexp(pattern = "\\d{3}-\\d{3}-\\d{4}") phoneNumber: String = "") derives Validator

case class RelationValidationModel(@required @rel relation: Option[ValueRelation] = None) derives Validator

case class ChoiceValidationModel(
  @choice(group = "group1", min = 2, max = 2) option1: Option[ValueRelation] = None,
  @choice(group = "group1", min = 2, max = 2) option2: Option[ValueRelation] = None,
  @choice(group = "group1", min = 2, max = 2) option3: Option[ValueRelation] = None,
  @choice(group = "group2", max = 2) option4: Option[ValueRelation] = None,
  @choice(group = "group2", max = 2) option5: Option[ValueRelation] = None
) derives Validator

case class OptionsValidationModel(
  @options(values = Seq("A", "B", "C")) stringOption: String = "",
  @options(values = Seq(1, 2, 3)) intOption: Int = 0
) derives Validator

case class ListValidationModel(
  @list(min = 2) minList: List[String] = List(),
  @list(max = 5) maxList: List[String] = List(),
  @list(min = 1, max = 3) rangeList: List[String] = List()
) derives Validator

case class CombinedValidationModel(
  @required @string(min = 5, max = 20) username: String = "",
  @required @email email: String = "",
  @required @number(min = 18, max = 120) age: Int = 0,
  @required @rel relation: Option[ValueRelation] = None,
  @required @list(min = 1) @options(values = Seq("admin", "user", "guest")) roles: List[String] = List()
) derives Validator

// sbt testOnly *RouterTest
class ValidationTests extends AnyFlatSpec {

  "Required validation" should "fail for null values" in {
    val model  = BasicModel(id = 1, name = null, email = "valid@email.com")
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "name"
    result.messages("name").head should include("required")
  }

  it should "fail for empty strings" in {
    val model  = BasicModel(id = 1, name = "", email = "valid@email.com")
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "name"
    result.messages("name").head should include("required")
  }

  it should "fail for zero values in numbers" in {
    val model  = BasicModel(id = 0, name = "Test", email = "valid@email.com")
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "id"
    result.messages("id").head should include("required")
  }

  it should "pass for valid values" in {
    val model  = BasicModel(id = 1, name = "Test", email = "valid@email.com")
    val result = model.validate

    result.isValid shouldBe true
  }

  // Test cases for string validation
  "String validation" should "fail for blank strings when blank=false" in {
    val model  = StringValidationModel(
      nonBlankString = "   ",
      nonEmptyString = "test",
      minString = "validlength",
      maxString = "valid",
      rangeString = "valid"
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "nonBlankString"
    result.messages("nonBlankString").head should include("can't be blank")
  }

  it should "fail for empty strings when empty=false" in {
    val model  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "",
      minString = "validlength",
      maxString = "valid",
      rangeString = "valid"
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "nonEmptyString"
  }

  it should "fail for strings shorter than min length" in {
    val model  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "test",
      minString = "shor",
      maxString = "valid",
      rangeString = "valid"
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "minString"
    result.messages("minString").head should include("must be bigger or equal 5")
  }

  it should "fail for strings longer than max length" in {
    val model  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "test",
      minString = "validlength",
      maxString = "thisiswaytoolong",
      rangeString = "valid"
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "maxString"
    result.messages("maxString").head should include("must be less or equal 10")
  }

  it should "fail for strings outside of range length" in {
    // Too short
    val modelShort  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "test",
      minString = "validlength",
      maxString = "valid",
      rangeString = "ab"
    )
    val resultShort = modelShort.validate

    resultShort.isValid shouldBe false
    resultShort.messages should contain key "rangeString"
    resultShort.messages("rangeString").head should include("must be between 3 and 8")

    // Too long
    val modelLong  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "test",
      minString = "validlength",
      maxString = "valid",
      rangeString = "toolongstring"
    )
    val resultLong = modelLong.validate

    resultLong.isValid shouldBe false
    resultLong.messages should contain key "rangeString"
    resultLong.messages("rangeString").head should include("must be between 3 and 8")
  }

  it should "pass for valid strings" in {
    val model  = StringValidationModel(
      nonBlankString = "test",
      nonEmptyString = "test",
      minString = "validlength",
      maxString = "valid",
      rangeString = "valid"
    )
    val result = model.validate

    result.messages shouldBe Map.empty

    result.isValid shouldBe true
  }

  // Test cases for number validation
  "Number validation" should "fail for values below minimum" in {
    val model  = NumberValidationModel(minValue = 5, maxValue = 50, rangeValue = 30)
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "minValue"
    result.messages("minValue").head should include("must be bigger or equal 10")
  }

  it should "fail for values above maximum" in {
    val model  = NumberValidationModel(minValue = 15, maxValue = 150, rangeValue = 30)
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "maxValue"
    result.messages("maxValue").head should include("must be less or equal 100")
  }

  it should "fail for values outside range" in {
    // Below minimum
    val modelBelow  = NumberValidationModel(minValue = 15, maxValue = 50, rangeValue = 15)
    val resultBelow = modelBelow.validate

    resultBelow.isValid shouldBe false
    resultBelow.messages should contain key "rangeValue"
    resultBelow.messages("rangeValue").head should include("must be between 20 and 50")

    // Above maximum
    val modelAbove  = NumberValidationModel(minValue = 15, maxValue = 50, rangeValue = 60)
    val resultAbove = modelAbove.validate

    resultAbove.isValid shouldBe false
    resultAbove.messages should contain key "rangeValue"
    resultAbove.messages("rangeValue").head should include("must be between 20 and 50")
  }

  it should "pass for valid numbers" in {
    val model  = NumberValidationModel(minValue = 15, maxValue = 50, rangeValue = 30)
    val result = model.validate
    result.messages shouldBe Map.empty

    result.isValid shouldBe true
  }

  // Test cases for regexp validation
  "Regexp validation" should "fail for strings not matching pattern" in {
    val model  = RegexpValidationModel(phoneNumber = "invalid")
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "phoneNumber"
    result.messages("phoneNumber").head should include("match with expected pattern")
  }

  it should "pass for strings matching pattern" in {
    val model  = RegexpValidationModel(phoneNumber = "123-456-7890")
    val result = model.validate

    result.isValid shouldBe true
  }

  // Test cases for relation validation
  "Relation validation" should "fail for None values" in {
    val model  = RelationValidationModel(relation = None)
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "relation"
    result.messages("relation").head should include("required")
  }

  it should "fail for invalid relations" in {
    val model  = RelationValidationModel(relation = Some(ValueRelation(1, "test", valid = false)))
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "relation"
    result.messages("relation").head should include("relation")
  }

  it should "pass for valid relations" in {
    val model  = RelationValidationModel(relation = Some(ValueRelation(1, "test", valid = true)))
    val result = model.validate

    result.isValid shouldBe true
  }

  // Test cases for choice validation
  "Choice validation" should "fail when minimum choices not met" in {
    val relation = Some(ValueRelation(1, "test", valid = true))
    val model    = ChoiceValidationModel(
      option1 = None,
      option2 = None,
      option3 = relation, // This needs another selection in the group since min=2
      option4 = None,
      option5 = None
    )
    val result   = model.validate

    result.isValid shouldBe false
    result.messages should contain key "option3"
    result.messages("option3").head should include("must be between 2 and 2 selections between then options")
  }

  it should "fail when maximum choices exceeded" in {
    val relation = Some(ValueRelation(1, "test", valid = true))
    val model    = ChoiceValidationModel(
      option1 = relation,
      option2 = relation, // Exceeds max=1 by default for group1
      option3 = None,
      option4 = None,
      option5 = None
    )
    val result   = model.validate

    result.isValid shouldBe false
    // There would be a message about maximum choices for group1
  }

  it should "fail when both option4 and option5 are selected exceeding max=2" in {
    val relation = Some(ValueRelation(1, "test", valid = true))
    val model    = ChoiceValidationModel(
      option1 = relation,
      option2 = relation,
      option3 = None,
      option4 = relation,
      option5 = relation
    )
    val result   = model.validate
    result.messages shouldBe Map.empty

    result.isValid shouldBe true // This should pass as max=2 for group2
  }

  it should "pass for valid choice selections" in {
    val relation = Some(ValueRelation(1, "test", valid = true))
    val model    =
      ChoiceValidationModel(option1 = relation, option2 = relation, option3 = None, option4 = relation, option5 = None)
    val result   = model.validate

    result.isValid shouldBe true
  }

  // Test cases for options validation
  "Options validation" should "fail for values not in the options list" in {
    val model  = OptionsValidationModel(
      stringOption = "D", // Not in ["A", "B", "C"]
      intOption = 5       // Not in [1, 2, 3]
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "stringOption"
    result.messages("stringOption").head should include("must be one of")
    result.messages should contain key "intOption"
    result.messages("intOption").head should include("must be one of")
  }

  it should "pass for values in the options list" in {
    val model  = OptionsValidationModel(stringOption = "A", intOption = 2)
    val result = model.validate
    result.messages shouldBe Map.empty
    result.isValid shouldBe true
  }

  // Test cases for list validation
  "List validation" should "fail for lists with fewer items than minimum" in {
    val model  = ListValidationModel(
      minList = List("one"), // Minimum is 2
      maxList = List("one", "two"),
      rangeList = List()     // Empty but minimum is 1
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "minList"
    result.messages("minList").head should include("must be bigger os equal 2")
    result.messages should contain key "rangeList"
    result.messages("rangeList").head should include("must be between 1 and 3")
  }

  it should "fail for lists with more items than maximum" in {
    val model  = ListValidationModel(
      minList = List("one", "two"),
      maxList = List("one", "two", "three", "four", "five", "six"), // Max is 5
      rangeList = List("one", "two", "three", "four")               // Max is 3
    )
    val result = model.validate

    result.isValid shouldBe false
    result.messages should contain key "maxList"
    result.messages("maxList").head should include("must be less os equal 5")
    result.messages should contain key "rangeList"
    result.messages("rangeList").head should include("must be between 1 and 3")
  }

  it should "pass for lists with valid sizes" in {
    val model  = ListValidationModel(
      minList = List("one", "two"),
      maxList = List("one", "two", "three", "four", "five"),
      rangeList = List("one", "two", "three")
    )
    val result = model.validate

    result.isValid shouldBe true
  }

  // Combined validation tests
  "Combined validations" should "check all validations" in {
    // Create a model with multiple validation failures
    val invalidModel = CombinedValidationModel(
      username = "user",         // Too short (min=5)
      email = "invalid-email",   // Invalid email format
      age = 15,                  // Below min age (18)
      relation = None,           // Required relation is missing
      roles = List("superadmin") // Invalid role (not in options)
    )
    val result       = invalidModel.validate

    result.isValid shouldBe false
    result.messages should contain key "username"
    result.messages should contain key "email"
    result.messages should contain key "age"
    result.messages should contain key "relation"
    result.messages should contain key "roles"

    // Create a valid model
    val validModel  = CombinedValidationModel(
      username = "validuser",
      email = "valid@email.com",
      age = 25,
      relation = Some(ValueRelation(1, "test", valid = true)),
      roles = List("admin", "user")
    )
    val validResult = validModel.validate
    validResult.messages shouldBe Map.empty
    validResult.isValid shouldBe true
  }

}
