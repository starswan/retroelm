# frozen_string_literal: true
#
# $Id$
#
require "rvm/capistrano"
set :rvm_ruby_string, "2.7.6@retroelm"
set :bundle_without, %i[development test]
#set :rvm_install_type, '1.26.10'
# Using distcc this number can maybe go higher
#set :rvm_install_ruby_threads, 3

# main details
set :application, "retroelm"
set :use_sudo, false

# repo details
# set :scm, :git # You can set :scm explicitly or Capistrano will make an intelligent guess based on known version control directory names
# Or: `accurev`, `bzr`, `cvs`, `darcs`, `git`, `mercurial`, `perforce`, `subversion` or `none`
# set :deploy_via, :copy
# set :scm, :subversion
# set :repository, "http://subversion/svn/starswan/trunk/projects/retro/retroelm"
set :scm, :git
set :repository, "git@github.com:starswan/qaop-elm.git"
set :branch, ENV['BRANCH'] || 'main'

set :deploy_to, "#{ENV['HOME']}/retroelm"
set :user, ENV['USER']
set :rails_env, "production"
# RVM now installed on arthur via an apt package
# set :rvm_type, "/usr/share/rvm"

# role :web, "your web-server here"                          # Your HTTP server, Apache/etc
# role :app, "your app-server here"                          # This may be the same as your `Web` server
# role :db,  "your primary db-server here", :primary => true # This is where Rails migrations will run
# role :db,  "your slave db-server here"
server "arthur", :web, :app, :db, :primary => true

# if you want to clean up old releases on each deploy uncomment this:
after "deploy:restart", "deploy:cleanup"
before "deploy:assets:precompile", "bundler:install"
after "deploy:update_code", "bundler:symlink_bundled_gems"

# If you are using Passenger mod_rails uncomment this:
namespace :deploy do
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "#{try_sudo} touch #{File.join(current_path,'tmp','restart.txt')}"
  end
end

namespace :bundler do
  desc "Symlink bundled gems on each release"
  task :symlink_bundled_gems, roles: :app do
    run "mkdir -p #{shared_path}/bundled_gems"
    run "ln -nfs #{shared_path}/bundled_gems #{release_path}/vendor/bundle"
  end

  desc "Install for production"
  task :install, roles: :app do
    run "cd #{release_path} && bundle install --without development test"
  end
end
