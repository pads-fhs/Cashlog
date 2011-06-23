INSERT INTO shop VALUES(null,'Media-Markt','Zella-Mehlis');
INSERT INTO shop VALUES(null,'Heinze-Bollek','Zella-Mehlis');
INSERT INTO shop VALUES(null,'Saturn','Erfurt');

INSERT INTO category VALUES(null,0,'Mainboard');
INSERT INTO category VALUES(null,0,'PCI-Grafikkarte');
INSERT INTO category VALUES(null,0,'PCIE-Grafikkarte');
INSERT INTO category VALUES(null,0,'DDR2-RAM');
INSERT INTO category VALUES(null,0,'DDR3-RAM');

INSERT INTO article VALUES(null,'MSI-Mainboard-II',299.99,1);
INSERT INTO article VALUES(null,'MSI-Mainboard-III',349.99,1);
INSERT INTO article VALUES(null,'ATI-SUPER-III',349.99,3);
INSERT INTO article VALUES(null,'ATI-SUPER-II',349.99,2);
INSERT INTO article VALUES(null,'NVIDIA-MEGA-I',349.99,2);
INSERT INTO article VALUES(null,'NVIDIA-MEGA-II',349.99,3);
INSERT INTO article VALUES(null,'Kingston-XX-I',349.99,4);
INSERT INTO article VALUES(null,'Kingston-XX-II',349.99,5);

INSERT INTO voucher VALUES(null,datetime('now'),1);
INSERT INTO voucher VALUES(null,datetime('now'),2);
INSERT INTO voucher VALUES(null,datetime('now'),3);

INSERT INTO position VALUES(null,1,3,2,700);
INSERT INTO position VALUES(null,1,8,2,700);
INSERT INTO position VALUES(null,1,5,2,700);

INSERT INTO position VALUES(null,2,3,2,700);
INSERT INTO position VALUES(null,2,8,2,700);
INSERT INTO position VALUES(null,2,5,2,700);

INSERT INTO position VALUES(null,3,3,2,700);
INSERT INTO position VALUES(null,3,8,2,700);
INSERT INTO position VALUES(null,3,5,2,700);
