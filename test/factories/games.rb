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
  end
end
