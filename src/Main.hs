module Main where

main = do
	c <- connectDatabase "cashlog.db"
	insertArticle c (-1, "Mainboard", 1.99, 1)
	disconnectDatabase c
