# frozen_string_literal: true
#
# $Id$
#
require "rvm/capistrano"
set :rvm_ruby_string, "2.6.7@retroelm"
set :bundle_without, %i[development test]
#set :rvm_install_type, '1.26.10'
# Using distcc this number can maybe go higher
#set :rvm_install_ruby_threads, 3

# main details
set :application, "retroelm"
set :deploy_via, :copy
set :use_sudo, false

# repo details
# set :scm, :git # You can set :scm explicitly or Capistrano will make an intelligent guess based on known version control directory names
# Or: `accurev`, `bzr`, `cvs`, `darcs`, `git`, `mercurial`, `perforce`, `subversion` or `none`
set :scm, :subversion
set :repository, "http://subversion/svn/starswan/trunk/projects/retro/retroelm"

set :deploy_to, "/home/steve/retroelm"
set :user, "steve"
set :rails_env, "production"
# RVM now installed on arthur via an apt package
set :rvm_type, "/usr/share/rvm"

# role :web, "your web-server here"                          # Your HTTP server, Apache/etc
# role :app, "your app-server here"                          # This may be the same as your `Web` server
# role :db,  "your primary db-server here", :primary => true # This is where Rails migrations will run
# role :db,  "your slave db-server here"
server "arthur.broadband", :web, :app, :db, :primary => true

# if you want to clean up old releases on each deploy uncomment this:
after "deploy:restart", "deploy:cleanup"

# If you are using Passenger mod_rails uncomment this:
namespace :deploy do
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "#{try_sudo} touch #{File.join(current_path,'tmp','restart.txt')}"
  end
end