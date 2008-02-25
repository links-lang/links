-- MySQL dump 10.11
--
-- Host: localhost    Database: ide
-- ------------------------------------------------------
-- Server version	5.0.38-Ubuntu_0ubuntu1.2-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `files`
--

CREATE TABLE `files` (
  `id` int(11) NOT NULL auto_increment,
  `pid` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  `last_mod` varchar(50) NOT NULL,
  `created` varchar(50) NOT NULL,
  `data` text,
  `parent` int(11) default NULL,
  `isfolder` int(1) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `pid` (`pid`),
  KEY `parent` (`parent`),
  CONSTRAINT `files_ibfk_1` FOREIGN KEY (`pid`) REFERENCES `projects` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!50003 SET @OLD_SQL_MODE=@@SQL_MODE*/;
DELIMITER ;;
/*!50003 SET SESSION SQL_MODE="" */;;
/*!50003 CREATE */ /*!50017 DEFINER=`root`@`localhost` */ /*!50003 TRIGGER `set_create_date` BEFORE INSERT ON `files` FOR EACH ROW begin  set NEW.created = now(), NEW.last_mod = now(); end */;;

/*!50003 SET SESSION SQL_MODE="" */;;
/*!50003 CREATE */ /*!50017 DEFINER=`root`@`localhost` */ /*!50003 TRIGGER `set_last_mod` BEFORE UPDATE ON `files` FOR EACH ROW begin  set NEW.last_mod = now(); end */;;

DELIMITER ;
/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;

--
-- Table structure for table `folders`
--

CREATE TABLE `folders` (
  `id` int(11) NOT NULL auto_increment,
  `pid` int(11) NOT NULL,
  `name` varchar(255) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `pid` (`pid`),
  CONSTRAINT `folders_ibfk_1` FOREIGN KEY (`pid`) REFERENCES `projects` (`id`),
  CONSTRAINT `folders_ibfk_2` FOREIGN KEY (`pid`) REFERENCES `projects` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `projects`
--

CREATE TABLE `projects` (
  `id` int(11) NOT NULL auto_increment,
  `uid` int(11) NOT NULL,
  `name` varchar(255) default NULL,
  `dirty` int(1) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `id` (`id`),
  KEY `uid` (`uid`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Table structure for table `sessions`
--

CREATE TABLE `sessions` (
  `id` varchar(32) NOT NULL,
  `uid` int(11) NOT NULL,
  `creation_time` int(11) NOT NULL,
  `update_time` int(11) NOT NULL,
  PRIMARY KEY  (`id`),
  KEY `uid` (`uid`),
  CONSTRAINT `sessions_ibfk_1` FOREIGN KEY (`uid`) REFERENCES `users` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

/*!50003 SET @OLD_SQL_MODE=@@SQL_MODE*/;
DELIMITER ;;
/*!50003 SET SESSION SQL_MODE="" */;;
/*!50003 CREATE */ /*!50017 DEFINER=`root`@`localhost` */ /*!50003 TRIGGER `set_create` BEFORE INSERT ON `sessions` FOR EACH ROW begin   set NEW.creation_time = unix_timestamp();   set NEW.update_time = unix_timestamp(); end */;;

/*!50003 SET SESSION SQL_MODE="" */;;
/*!50003 CREATE */ /*!50017 DEFINER=`root`@`localhost` */ /*!50003 TRIGGER `update_session` BEFORE UPDATE ON `sessions` FOR EACH ROW begin declare cuid, ctime int; declare a cursor for select uid, creation_time from ide.sessions where id=NEW.id limit 1; open a; fetch a into cuid, ctime; set NEW.update_time = unix_timestamp(), NEW.uid = cuid, NEW.creation_time = ctime; close a; end */;;

DELIMITER ;
/*!50003 SET SESSION SQL_MODE=@OLD_SQL_MODE */;

--
-- Table structure for table `users`
--

CREATE TABLE `users` (
  `id` int(11) NOT NULL auto_increment,
  `email` char(255) NOT NULL,
  `password` char(32) NOT NULL,
  PRIMARY KEY  (`id`),
  UNIQUE KEY `id` (`id`),
  UNIQUE KEY `email` (`email`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Temporary table structure for view `valid_sessions`
--

/*!50001 CREATE TABLE `valid_sessions` (
  `id` varchar(32),
  `uid` int(11)
) */;

--
-- Final view structure for view `valid_sessions`
--

/*!50001 CREATE ALGORITHM=UNDEFINED */
/*!50013 DEFINER=`root`@`localhost` SQL SECURITY DEFINER */
/*!50001 VIEW `valid_sessions` AS select `sessions`.`id` AS `id`,`sessions`.`uid` AS `uid` from `sessions` where (`ide`.`check_session`(`sessions`.`id`) = 1) */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2008-02-25 10:59:26
