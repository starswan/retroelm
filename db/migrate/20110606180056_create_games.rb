class CreateGames < ActiveRecord::Migration[5.1]
  def self.up
    create_table :games do |t|
      t.string :name
      t.string :tapfile

      t.timestamps
    end
  end

  def self.down
    drop_table :games
  end
end
