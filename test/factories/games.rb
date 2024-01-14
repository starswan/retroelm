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
  end
end
