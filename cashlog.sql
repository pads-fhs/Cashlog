CREATE TABLE category (
id          INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
parent      INTEGER,
name        TEXT UNIQUE NOT NULL
);

CREATE TABLE shop (
id          INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
name        TEXT NOT NULL,
city        TEXT NOT NULL
);

CREATE TABLE article (
id          INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
name        TEXT NOT NULL,
price       REAL NOT NULL,
category_id INTEGER NOT NULL
	CONSTRAINT fk_article_category REFERENCES category(id)
);

CREATE TABLE voucher (
id          INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
timestamp   INTEGER NOT NULL,
shop_id     INTEGER NOT NULL
);

CREATE TABLE position (
id          INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE NOT NULL,
voucher_id  INTEGER NOT NULL
	CONSTRAINT fk_position_voucher REFERENCES voucher(id),
article_id  INTEGER NOT NULL
	CONSTRAINT fk_position_article REFERENCES article(id),
quantity    REAL NOT NULL,
price       REAL NOT NULL
);


CREATE UNIQUE INDEX IF NOT EXISTS index_shop ON shop (name, city);

-- Foreign Key Preventing insert
CREATE TRIGGER fki_article_category_id_category_id
BEFORE INSERT ON [article]
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'insert on table "article" violates foreign key constraint "fki_article_category_id_category_id"')
  WHERE (SELECT id FROM category WHERE id = NEW.category_id) IS NULL;
END;

-- Foreign key preventing update
CREATE TRIGGER fku_article_category_id_category_id
BEFORE UPDATE ON [article] 
FOR EACH ROW BEGIN
    SELECT RAISE(ROLLBACK, 'update on table "article" violates foreign key constraint "fku_article_category_id_category_id"')
      WHERE (SELECT id FROM category WHERE id = NEW.category_id) IS NULL;
END;

-- Foreign key preventing delete
CREATE TRIGGER fkd_article_category_id_category_id
BEFORE DELETE ON category
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'delete on table "category" violates foreign key constraint "fkd_article_category_id_category_id"')
  WHERE (SELECT category_id FROM article WHERE category_id = OLD.id) IS NOT NULL;
END;

-- Foreign Key Preventing insert
CREATE TRIGGER fki_position_voucher_id_voucher_id
BEFORE INSERT ON [position]
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'insert on table "position" violates foreign key constraint "fki_position_voucher_id_voucher_id"')
  WHERE (SELECT id FROM voucher WHERE id = NEW.voucher_id) IS NULL;
END;

-- Foreign key preventing update
CREATE TRIGGER fku_position_voucher_id_voucher_id
BEFORE UPDATE ON [position] 
FOR EACH ROW BEGIN
    SELECT RAISE(ROLLBACK, 'update on table "position" violates foreign key constraint "fku_position_voucher_id_voucher_id"')
      WHERE (SELECT id FROM voucher WHERE id = NEW.voucher_id) IS NULL;
END;

-- Foreign key preventing delete
CREATE TRIGGER fkd_position_voucher_id_voucher_id
BEFORE DELETE ON voucher
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'delete on table "voucher" violates foreign key constraint "fkd_position_voucher_id_voucher_id"')
  WHERE (SELECT voucher_id FROM position WHERE voucher_id = OLD.id) IS NOT NULL;
END;

-- Foreign Key Preventing insert
CREATE TRIGGER fki_position_article_id_article_id
BEFORE INSERT ON [position]
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'insert on table "position" violates foreign key constraint "fki_position_article_id_article_id"')
  WHERE (SELECT id FROM article WHERE id = NEW.article_id) IS NULL;
END;

-- Foreign key preventing update
CREATE TRIGGER fku_position_article_id_article_id
BEFORE UPDATE ON [position] 
FOR EACH ROW BEGIN
    SELECT RAISE(ROLLBACK, 'update on table "position" violates foreign key constraint "fku_position_article_id_article_id"')
      WHERE (SELECT id FROM article WHERE id = NEW.article_id) IS NULL;
END;

-- Foreign key preventing delete
CREATE TRIGGER fkd_position_article_id_article_id
BEFORE DELETE ON article
FOR EACH ROW BEGIN
  SELECT RAISE(ROLLBACK, 'delete on table "article" violates foreign key constraint "fkd_position_article_id_article_id"')
  WHERE (SELECT article_id FROM position WHERE article_id = OLD.id) IS NOT NULL;
END;
