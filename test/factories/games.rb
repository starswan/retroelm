FactoryBot.define do
  factory :game do
    trait :one do
        name { 'MyString' }
      tapfile { 'MyString' }
    end

    trait :two do
      name { 'MyString' }
      tapfile { 'MyString' }
    end

    trait :match_day do
      name { 'Match Day' }
      tapfile { 'MATCHDAY.tap' }
    end

    trait :z80_test_full do
      name { 'Z80 Full Test' }
      tapfile { 'z80test/z80full.tap' }
    end
  end
end
