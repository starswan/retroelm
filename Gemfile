# yet again my gems are not a secret from anyone

source 'http://rubygems.org'
git_source(:github) { |repo| "https://github.com/#{repo}.git" }

gem "base64"
gem "bigdecimal"
gem "drb"
gem "mutex_m"

# Bundle edge Rails instead: gem 'rails', github: 'rails/rails'
gem 'rails', '~> 7.0', '< 7.1'
# Use sqlite3 as the database for Active Record
gem 'sqlite3', '~> 2.6'

# mysql in production
gem 'mysql2'
# Use Passenger as the app server
gem "passenger", "~> 5.0"
# Use Puma as the app server
# gem 'puma', '~> 4.1'
# Use SCSS for stylesheets
gem 'sass-rails', '>= 6'
# Build JSON APIs with ease. Read more: https://github.com/rails/jbuilder
gem 'jbuilder', '~> 2.7'
# Use Active Model has_secure_password
# gem 'bcrypt', '~> 3.1.7'

# Use Active Storage variant
# gem 'image_processing', '~> 1.2'
gem 'nokogiri', '< 1.19'

# Reduces boot times through caching; required in config/boot.rb
# gem 'bootsnap', '>= 1.4.2', require: false
gem 'matrix', require: false

# gem 'smtp'
# This is something to do with ruby 3.1...?
gem 'net-smtp', require: false
gem 'rexml', require: false
gem 'rubyzip', '< 3'

group :development, :test do
  # Call 'byebug' anywhere in the code to stop execution and get a debugger console
  gem 'byebug', platforms: [:mri, :mingw, :x64_mingw]

  gem "capybara"

  gem "faraday", ">= 2"
  gem "faraday-follow_redirects"
  # gem "faraday_middleware"

  gem "guard-bundler"
  gem "guard-rspec"
  # gem "guard-rubocop"

  gem 'rspec-rails'
  gem "puma"
end

group :development do
  # Access an interactive console on exception pages or by calling 'console' anywhere in the code.
  # gem 'web-console', '>= 3.3.0'
  # gem 'listen', '~> 3.9'
  # Spring speeds up development by keeping your application running in the background. Read more: https://github.com/rails/spring
  # gem 'spring'
  # gem 'spring-watcher-listen', '~> 2.0.0'

  # Use Capistrano for deployment
  # gem "capistrano", "~> 2.15.8"
  gem "capistrano"
  gem "capistrano-ext"
  gem "capistrano-rails"
  gem "rvm-capistrano", require: false

  gem 'ed25519'
  gem 'bcrypt_pbkdf'
end

group :test do
  # Adds support for Capybara system testing and selenium driver
  # gem 'capybara', '>= 2.15'
  # 4.11 insists that /usr/bin/firefox is a binary, when it isn't
  # on Ubuntu 22.04 due to it being a snap which is very annoying
  # gem "selenium-webdriver", "< 4.11"
  # gem 'geckodriver'
  # cuprite driving chrom(ium) is much nicer
  gem "cuprite"

  gem "factory_bot_rails"

  gem 'simplecov', require: false
  gem 'rails-controller-testing'
end

# Windows does not include zoneinfo files, so bundle the tzinfo-data gem
gem 'tzinfo-data', platforms: [:mingw, :mswin, :x64_mingw, :jruby]

gem "jsbundling-rails", "~> 1.3"
