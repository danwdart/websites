CREATE DATABASE IF NOT EXISTS `visits`;
GRANT ALL ON `visits`.`*` TO `visits`;
CREATE TABLE IF NOT EXISTS `visits`.`visits` (
    `id` INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `url` TEXT NOT NULL,
    `ua` TEXT NOT NULL,
    `ip` TEXT NOT NULL,
    `time` TIMESTAMP NOT NULL
);
CREATE TABLE IF NOT EXISTS `visits`.`adminvisits` (
    `id` INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `url` TEXT NOT NULL,
    `ua` TEXT NOT NULL,
    `ip` TEXT NOT NULL,
    `time` TIMESTAMP NOT NULL
);
CREATE DATABASE IF NOT EXISTS `newsletters`;
CREATE TABLE IF NOT EXISTS `newsletters`.`emails` (
    `id` INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
    `email` VARCHAR(255) NOT NULL,
    `active` BOOLEAN DEFAULT TRUE,
    `created_at` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    `updated_at` TIMESTAMP
);

ALTER TABLE `visits`.`visits` ADD `page` TEXT;
ALTER TABLE `visits`.`visits` ADD `sub` TEXT;