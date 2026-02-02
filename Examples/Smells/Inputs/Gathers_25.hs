module Types where

import Data.List

data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState = PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y = x   {-
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 
   -}


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )


printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = []
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- * Funções adicionadas

-- | Função que dá as coordenadas do jogador.
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Função que dá o número de vidas do jogador.
getPlayerLives :: Player -> Int
getPlayerLives (Pacman (PacState (x,y,z,t,h,l) q c d )) = l



-- | Função que devolve a lista de coordenadas duma lista de players.
getPlayersCoords :: [Player] -> [Coords]
getPlayersCoords [] = []
getPlayersCoords (x:xs) = getPlayerCoords x : getPlayersCoords xs

-- | Função que junta os players que são dead ghosts.
deadGhosts :: [Player] -> [Player]
deadGhosts [] = []
deadGhosts (p@(Pacman (PacState (a,b,c,d,e,f) g h i)) : ps) = deadGhosts ps
deadGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)) : ps) = deadGhosts ps
deadGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Dead)) : ps) = p : deadGhosts ps

-- | Função que dá as coordenadas dos Dead Ghosts que estão num determinado State.
ghostsDCoords :: State -> [Coords]
ghostsDCoords (State m x l) = getPlayersCoords (deadGhosts x)

-- | Função que junta os players que são Alive Ghosts.
aliveGhosts :: [Player] -> [Player]
aliveGhosts [] = []
aliveGhosts ((Pacman (PacState (a,b,c,d,e,f) g h i)) : ps) = aliveGhosts ps
aliveGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)) : ps) = p : aliveGhosts ps
aliveGhosts ((Ghost (GhoState (a,b,c,d,e,f) Dead)) : ps) = aliveGhosts ps

-- | Função que dá as coordenadas dos ghosts que estão num determinado State.
ghostsAlCoords :: State -> [Coords]
ghostsAlCoords (State m x l) = getPlayersCoords (aliveGhosts x)

-- | Função que verifica se um jogador é um Dead Ghost.
isDeadGhost :: Player -> Bool
isDeadGhost (Ghost (GhoState (a,b,c,d,e,f) Dead)) = True
isDeadGhost (Ghost (GhoState (a,b,c,d,e,f) Alive)) = False
isDeadGhost (Pacman (PacState (a,b,c,d,e,f) g h i)) = False

-- | Função que verifica se um conjunto de jogadores tem um Dead Ghost.
areDeadGhost :: [Player] -> Bool
areDeadGhost [] = False
areDeadGhost ((Ghost (GhoState (a,b,c,d,e,f) Dead)):xs) = True
areDeadGhost (x:xs) = areDeadGhost xs 

-- | Devolve a coordenada do interior da casa dos fantasmas relativamente a um dado labirinto, usada para mandar os fantasmas comidos para lá.
midCoord :: Maze   -- ^ Labirinto.
         -> Coords -- ^ Coordenadas da localização do interior da casa dos fantasmas no respetivo labirinto.
midCoord m@(x:xs) | even lm = ((div lm 2)-1, div lx 2)
                  | otherwise = (div lm 2, div lx 2)
                                where lm = length m -- Altura do labirinto.
                                      lx = length x -- Comprimento do labirinto.

-- | Dada uma lista de Players, devolve a lista das coordenadas de todos os fantasmas.
ghostsCoords :: [Player] -> [Coords]
ghostsCoords l = getPlayersCoords (ghostPlayers l)

-- | Dada uma lista de Players devolve o Player que é Pacman.
thePacman :: [Player] -> Player
thePacman (x:xs) | isPac x = x
                 | otherwise = thePacman xs

-- | Se o Player recebido for Pacman, devolve True, caso contrário devolve False.
isPac :: Player -> Bool
isPac (Pacman (PacState (a,b,c,d,e,f) g h i)) = True
isPac x = False

-- | Se o Player recebido for Ghost, devolve True, caso contrário devolve False.
isGhost :: Player -> Bool
isGhost (Ghost (GhoState (a,b,c,d,e,f) g)) = True
isGhost x = False

-- | Função que de um conjunto de Players, devolve o Player com o ID pedido.
idPlayer :: [Player] -> Int -> Player
idPlayer (x:xs) n | getPlayerID x == n = x
                  | otherwise = idPlayer xs n

-- | Função que devolve a lista de Ghosts que estão numa lista de Players.
ghostPlayers :: [Player] -> [Player]
ghostPlayers [] = []
ghostPlayers (x:xs) | isGhost x = x : ghostPlayers xs
                    | otherwise = ghostPlayers xs

-- | Função que diz em que coordenadas o Pacman se encontra no labirinto.
pacCoords :: State -> Coords
pacCoords (State m (x:xs) l) | isPac x = getPlayerCoords x
                             | otherwise = pacCoords (State m xs l)

-- | Função que devolve a coordenada imediatamente abaixo da coordenada recebida.
coo1D :: Coords -> Coords
coo1D (x,y) = (x+1,y)

-- | Dado uma Piece e um State, vai utilizar a função auxiliar, tendo como acumulador inicial (0,0), para determinar a lista das coordenadas da Piece dada.
getPieceCoords :: Piece -> State -> [Coords]
getPieceCoords p (State m x l) = auxGetPcCoords p m (0,0)


-- | Função auxiliar da função anterior que dado uma Piece, um Maze e Coords (acumulador, que será (0,0) para que determine corretamente as coordenadas do labirinto), devolvendo a lista de coordenadas onde a respetiva peça se encontra no labirinto.
auxGetPcCoords :: Piece -> Maze -> Coords -> [Coords]
auxGetPcCoords p [] (acx,acy) = []
auxGetPcCoords p ([]:ys) (acx,acy) = auxGetPcCoords p ys (acx+1,0)
auxGetPcCoords p ((x:xs):ys) (acx,acy) | p == x = (acx,acy) : auxGetPcCoords p (xs:ys) (acx,acy+1)
                                       | otherwise = auxGetPcCoords p (xs:ys) (acx,acy+1)
{- |

= Introdução

Esta tarefa consistiu em desenvolver um bot que fosse capaz de jogar sozinho sem nenhum input humano.


= Objetivos

O objetivo desta tarefa era dar jogadas ao Pacman, tendo em conta o estado do jogo e do mapa. Definimos como objetivos ir atrás do fantasmas mortos, fugir de fantasmas vivos, e quando estiver perto, comer uma Big Food.
Como objetivo principal estabelecemos que era fugir a fantasmas vivos, priorizando estar vivo a obter pontos, para tal o bot verifica se existe algum fantasma vivo a aproximar-se dele e caso haja, ele tenta ir na direção oposta para fugir do perigo.
Secundariamente, ou seja, caso não houvesse perigo por perto, o bot iria atrás de fantasmas mortos, que dão uma maior pontuação e eliminam a ameaça do mapa (temporariamente).
Caso não detete nenhuma destas circunstâncias, ele irá tentar ir atrás de comidas grandes para conseguir aumentar a sua pontuação e forçar os ghosts a ficarem Dead. 

= Discussão e conclusão

Apesar de não ser um bot extremamente inteligente, com uma inteligência artificial espetacular, dá para ver que é capaz de cumprir os seus objetivos fugindo do perigo e indo atrás de oportunidades para pontuar.
Esta tarefa foi uma tarefa trabalhosa, mas, tal como na Tarefa 5, é muito satisfatório ver o código que se escreveu ser posto em prática ao testar e ver os fantasmas e o Pacman uns atrás dos outros, aplicando as estratégias que desenhamos para eles.

-}




-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades à esquerda.
neargL :: Coords -> [Coords] -> Bool
neargL _ [] = False
neargL (x,y) ((x1,y1):xs) | (y-y1) <= 4 && y1 < y = True
                          | otherwise = neargL (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades à direita.
neargR :: Coords -> [Coords] -> Bool
neargR _ [] = False
neargR (x,y) ((x1,y1):xs) | (y1-y) <= 4 && y < y1 = True
                          | otherwise = neargR (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades em cima.
neargU :: Coords -> [Coords] -> Bool
neargU _ [] = False
neargU (x,y) ((x1,y1):xs) | (x-x1) <= 4 && x1 < x = True
                          | otherwise = neargU (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades em baixo.
neargD :: Coords -> [Coords] -> Bool
neargD _ [] = False
neargD (x,y) ((x1,y1):xs) | (x1-x) <= 4 && x < x1 = True
                          | otherwise = neargD (x,y) xs



-- | Função auxiliar que dado um State, um Int, e as coordenadas do Pacman no labirinto, devolve a jogada a fazer pelo bot.
coordsMPl :: State -> Int -> Coords -> Maybe Play
coordsMPl s@(State m p l) id c@(x,y) | c == midCoord m = Just (Move id U)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R && nxtWall c m U = Just (Move id D)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R && nxtWall c m D = Just (Move id U)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R = Just (Move id D)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L && nxtWall c m U = Just (Move id D)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L && nxtWall c m D = Just (Move id U)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L = Just (Move id D)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D && nxtWall c m L = Just (Move id R)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D && nxtWall c m R = Just (Move id L)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D = Just (Move id R)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U && nxtWall c m L = Just (Move id R)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U && nxtWall c m R = Just (Move id L)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U = Just (Move id R)
                                     | neargL c (ghostsAlCoords s) && neargR c (ghostsAlCoords s)    = Just (Move id U)
                                     | neargU c (ghostsAlCoords s) && neargD c (ghostsAlCoords s)    = Just (Move id R)
                                     | neargL c (ghostsAlCoords s) = Just (Move id R)
                                     | neargR c (ghostsAlCoords s) = Just (Move id L)
                                     | neargU c (ghostsAlCoords s) = Just (Move id D)
                                     | neargD c (ghostsAlCoords s) = Just (Move id U)
                                     | neargL c (ghostsDCoords s)  = Just (Move id L)
                                     | neargR c (ghostsDCoords s)  = Just (Move id R)
                                     | neargU c (ghostsDCoords s)  = Just (Move id U)
                                     | neargD c (ghostsDCoords s)  = Just (Move id D)
                                     | nxtWall (x,y) m orient      = Just (Move id (turnR orient))
                                     | otherwise = chaseBF (x,y) (getPieceCoords (Food Big) s) id orient
                                         where orient = getPlayerOrientation (idPlayer p id)


-- | Função que dada uma orientação, dá a orientação seguinte, no sentido dos ponteiros do relógio.
turnR :: Orientation -> Orientation
turnR R = D
turnR D = L
turnR L = U
turnR U = R
turnR Null = Null


-- | Função que analisa as coordenadas do Pacman e dá a Play a realizar para ir atrás de uma das coordenadas das peças de comida grande que se encontra próximo. Caso não haja nenhuma peça próxima ele irá continuar continuar com a sua orientação. 
chaseBF :: Coords      -- ^ Coordenadas do Pacman
        -> [Coords]    -- ^ Conjunto de coordenadas das peças de comida grande
        -> Int         -- ^ ID do Pacman
        -> Orientation -- ^ Orientação atual do Pacman
        -> Maybe Play  -- ^ Play a fazer tendo em conta a análise das coordenadas
chaseBF (x,y) c id or | neargL (x,y) c = Just (Move id L)
                      | neargR (x,y) c = Just (Move id R)
                      | neargU (x,y) c = Just (Move id U)
                      | neargD (x,y) c = Just (Move id D)
                      | otherwise = Just (Move id or)



-- | Função final que recebe um ID e um State e utilizando as funções auxiliares anteriores, devolve a Play a fazer pelo Pacman bot.
bot :: Int -> State -> Maybe Play
bot id s@(State m p l) = coordsMPl s id cs
                    where cs = getPlayerCoords (idPlayer p id)
{- |

= Introdução

Esta tarefa consistiu em implementar um comportamento para os fantasmas. Quer para fugir do Pacman, quer para ir atrás do Pacman. A tarefa inicialmente aparentou ser um pouco intimidadora, uma vez que teríamos de ter várias possibilidades e variáveis em consideração para determinar a melhor jogada possível.

= Objetivos

Inicialmente foi necessário fazer alterações na Tarefa 2 para admitir o movimento dos fantasmas o que provou ser um pouco trabalhoso, uma vez que implicou fazer várias alterações em várias funções da Tarefa 2 para assegurar o correto funcionamento do jogo. 

Para saber que jogada fazer, é necessário saber as coordenadas do fantasma (em questão) e do Pacman. Para isso elaboramos novas funções e usamos também algumas previamente feitas para conseguir extrair as coordenadas dos Players corretamente.
Após ter as funções de extração de coordenadas, era necessário funções que analisassem essas coordenadas e a sua contextualização no labirinto para que após a análise, devolvesse uma jogada a fazer.
Para tal, foram feitas funções seguindo as sugestões do enunciado do trabalho prático, as quais sugeriam que tentassemos determinar a direção a tomar para o fantasma se aproximar o máximo possível do Pacman.
Após os professores terem anunciado que tinhamos maior liberdade do que estava previsto no enunciado, decidimos melhorar um pouco mais a estratégia dos fantasmas adicionando mais algumas possibilidades, que não estavam a ser previstas pelo fantasma, tendo sido melhorada minimamente a eficácia dos fantasmas, relativamente à estratégia utilizada anteriormente.

Com a inteligência dos fantasmas quisemos que eles fossem desafiantes, mas também não queriamos que os fantasmas fossem extremamente eficazes a ir atrás do Pacman, algo que poderia ser frustrante para o jogador.

= Discussão e conclusão

Foi divertido ver nesta tarefa o resultado das linhas de código que escrevemos em ação e fugirmos dos fantasmas, brincando com o jogo para o testar, tal como se estivessemos a jogar o jogo original do Pacman.
Ficamos satisfeitos com o desempenho da inteligência artificial dos fantasmas, apesar de ter demonstrado ser uma tarefa trabalhosa e complicada.

-}




-- | Função que diz em que coordenadas está o Ghost com o determinado ID.
ghostIDCoords :: State -> Int -> Coords
ghostIDCoords (State m x l) id = getPlayerCoords (idPlayer x id)


-- | Função que dá a coordenada para a determinada orientação que é recebida como argumento, relativamente à coordenada inicial pedida.
chCoord :: Coords -> Maze -> Orientation -> Coords
chCoord (x,y) (m:ms) L | (y-1) < 0 = (x,(length m)-1)
                       | otherwise = (x,y-1)
chCoord (x,y) (m:ms) R | (y+1) > ((length m)-1) = (x,0)
                       | otherwise = (x,y+1)
chCoord (x,y) (m:ms) U = (x-1,y)
chCoord (x,y) (m:ms) D = (x+1,y) 
chCoord (x,y) (m:ms) Null = (x,y)


-- | Função que testa se há uma parede no sitio para onde o Player iria tendo em conta a orientação recebida como argumento.
nxtWall :: Coords -> Maze -> Orientation -> Bool
nxtWall c m or | pieceInCoords (chCoord c m or) m == Wall = True
               | otherwise = False


-- | Dada uma coordenada e uma orientação, devolve o conjunto das coordenadas, que se encontram 5 unidades de distancia relativamente à orientação fornecida.
listCoords6 :: Coords -> Orientation -> [Coords]
listCoords6 (x,y) L    = [(x,y-1), (x,y-2), (x,y-3), (x,y-4), (x,y-5)]
listCoords6 (x,y) R    = [(x,y+1), (x,y+2), (x,y+3), (x,y+4), (x,y+5)]
listCoords6 (x,y) U    = [(x-1,y), (x-2,y), (x-3,y), (x-4,y), (x-5,y)]
listCoords6 (x,y) D    = [(x+1,y), (x+2,y), (x+3,y), (x+4,y), (x+5,y)]
listCoords6 (x,y) Null = [(x,y+1), (x,y+2), (x,y+3), (x,y+4), (x,y+5)]


-- | Recebe o conjunto de coordenadas, produzido pela função anterior e filtra as coordenadas que se encontram fora do labirinto e que incluam as paredes que cobrem o labirinto, e pára de construir a lista quando uma das coordenadas coincide com as do Pacman.
filterCoords :: [Coords] -- ^ Conjunto de coordenadas criadas pela função anterior. 
             -> Maze     -- ^ Labirinto onde se enquadram as coordenadas.
             -> Coords   -- ^ Coordenadas da posição do Pacman.
             -> [Coords] -- ^ Resultado final filtrado pelas restrições programadas.
filterCoords [] m _ = []
filterCoords (a@(x,y):xs) mz@(m:ms) cp | a == cp = []
                                       | x <= 0 || x >= ((length mz)-1) || y <= 0 || y >= ((length m)-1) = []
                                       | otherwise = a : filterCoords xs mz cp


-- | Verifica (com uma função da Tarefa2) cada coordenada da lista filtrada anteriormente e se houver alguma coordenada em que haja uma Wall no labirinto, devolve False.
checkRetaWall :: [Coords] -> Maze -> Bool
checkRetaWall [] m = True
checkRetaWall (a@(x,y):xs) m | pieceInCoords a m == Wall = False
                             | otherwise = checkRetaWall xs m 


-- | Função que utiliza todas as funções auxiliares anteriores, para verificar se o fantasma pode ir atrás do Pacman sem ir contra nenhuma parede.
checkForWalls :: Coords      -- ^ Coordenadas do Ghost. 
              -> Orientation -- ^ Orientação para a qual quer verificar se há paredes.
              -> Maze        -- ^ Labirinto onde se enquadram as coordenadas.
              -> Coords      -- ^ Coordenadas do Pacman.
              -> Bool        -- ^ Retornará True se o caminho estiver livre, caso contrário retornará False.
checkForWalls c or m cp = checkRetaWall (filterCoords (listCoords6 c or) m cp) m



-- | Função auxiliar que dado um State, um ID, as coordenadas do Pacman e do Ghost com o determinado ID, decide a jogada a fazer para o Ghost ir atrás do Pacman.
coordsChase :: State  -- ^ Estado do jogo.
            -> Int    -- ^ ID de um determinado fantasma.
            -> Coords -- ^ Coordenadas do Pacman.
            -> Coords -- ^ Coordenadas do fantasma com o determinado ID.
            -> Play   -- ^ Play a executar pelo fantasma com o respetivo ID com a intenção de ir atrás do Pacman.
coordsChase s@(State m x l) id cp@(x1,y1) c@(x2,y2) 
                                 | getPlayerCoords (idPlayer x id) == midCoord m || getPlayerCoords (idPlayer x id) == coo1D (midCoord m) = (Move id U)  -- para sair da casa dos fantasmas
                                 | nxtWall gcoords m L && nxtWall gcoords m U && nxtWall gcoords m D = (Move id R)
                                 | nxtWall gcoords m R && nxtWall gcoords m U && nxtWall gcoords m D = (Move id L)
                                 | nxtWall gcoords m L && nxtWall gcoords m R && nxtWall gcoords m D = (Move id U)
                                 | nxtWall gcoords m L && nxtWall gcoords m R && nxtWall gcoords m U = (Move id D)
                                 | nxtWall gcoords m orient && x1 == x2 && y1 > y2 = (Move id R)
                                 | nxtWall gcoords m orient && x1 == x2 && y1 < y2 = (Move id L)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 == y2 = (Move id U)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 == y2 = (Move id D)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 < y2 && (x2-x1) >= (y2-y1) = (Move id U)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 < y2  = (Move id L)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 > y2 && (x1-x2) >= (y1-y2) = (Move id D)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 > y2  = (Move id R)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 > y2 && (x2-x1) >= (y2-y1) = (Move id U)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 > y2  = (Move id R)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 < y2 && (x1-x2) >= (y2-y1) = (Move id D)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 < y2  = (Move id L)
                                 | x1 == x2 && y1 > y2 && (y1-y2) <= 6 && checkForWalls c R m cp = (Move id R)
                                 | x1 == x2 && y1 < y2 && (y2-y1) <= 6 && checkForWalls c L m cp = (Move id L)
                                 | x1 < x2 && y1 == y2 && (x2-x1) <= 6 && checkForWalls c U m cp = (Move id U)
                                 | x1 > x2 && y1 == y2 && (x1-x2) <= 6 && checkForWalls c D m cp = (Move id D)
                                 | otherwise = (Move id orient)
                                     where orient = getPlayerOrientation (idPlayer x id)
                                           gcoords = getPlayerCoords (idPlayer x id)



-- | Função final que utiliza a função auxiliar anterior para dar um determinado ordem (Play) ao fantasma com o determinado ID, para ir atrás do Pacman.
chaseMode :: State -> Int -> Play
chaseMode s id = coordsChase s id (pacCoords s) (ghostIDCoords s id)


-- | Função auxiliar que dado um State, um ID, as coordenadas do Pacman e do Ghost com o determinado ID, decide a jogada a fazer para o Ghost fugir do Pacman.
coordsScatter :: State  -- ^ Estado do jogo.
              -> Int    -- ^ ID de um determinado fantasma.
              -> Coords -- ^ Coordenadas do Pacman.
              -> Coords -- ^ Coordenadas do fantasma com o determinado ID.
              -> Play   -- ^ Play a executar pelo fantasma com o respetivo ID com a intenção de fugir do Pacman.
coordsScatter s@(State m x l) id (x1,y1) (x2,y2) 
                                   | nxtWall gcoords m orient && orient == R = (Move id D)
                                   | nxtWall gcoords m orient && orient == D = (Move id L)
                                   | nxtWall gcoords m orient && orient == L = (Move id U)
                                   | nxtWall gcoords m orient && orient == U = (Move id R)
                                   | otherwise = (Move id orient)
                                       where orient = getPlayerOrientation (idPlayer x id)
                                             gcoords = getPlayerCoords (idPlayer x id)

-- | Função final que utiliza a função auxiliar anterior para dar um determinado ordem(Play) ao fantasma com o determinado ID, para fugir do Pacman.
scatterMode :: State -> Int -> Play
scatterMode s id = coordsScatter s id (pacCoords s) (ghostIDCoords s id)



-- | Função final adaptada para funcionar na Tarefa 4.
ghostPlayAux :: [Player] -> State -> [Play]
ghostPlayAux [] (State m z l) = []
ghostPlayAux (x:xs) s@(State m z l) | isPac x = ghostPlayAux xs (State m z l)
                                    | isDeadGhost x = scatterMode s (getPlayerID x) : ghostPlayAux xs (State m xs l)
                                    | otherwise = chaseMode s (getPlayerID x) : ghostPlayAux xs (State m xs l)



-- | Função final que dado um State, retorna o conjunto de Plays a fazer pelos jogadores do State.
ghostPlay :: State -> [Play]
ghostPlay (State m [] l) = []
ghostPlay s@(State m (x:xs) l) | isPac x = ghostPlay (State m xs l)
                               | isDeadGhost x = scatterMode s (getPlayerID x) : ghostPlay (State m xs l)
                               | otherwise = chaseMode s (getPlayerID x) : ghostPlay (State m xs l)

{- | 

= Introdução

O objetivo desta tarefa foi calcular o efeito da passagem de um instante de tempo num estado do jogo. 
A princípio esta tarefa pareceu assustadora, uma vez que introduz o conceito de passagem de tempo, mas com a ajuda do professor e alguma discussão entre colegas, conseguimos desenvolver estratégias para começarmos a fazer a tarefa.

= Objetivos

Nesta tarefa, foi usada uma unidade temporal de 250 ms por iteração. Usamos uma função que, dado um State e uma lista de Players, vai determinar a melhor jogada a ser efetuada para todos os players, analisando esse mesmo State.

Inicialmente criamos uma função que só fazia com que todos os Players do State andassem para a direita, como sugestão do professor.
Mais tarde, após a conclusão da Tarefa 5, melhoramos essa função implementando a obrigatoriedade de fazer o conjunto de jogadas que foram determinadas na Tarefa5 e fazer andar o Pacman na direção que ele tinha, sendo essa direção alterada pelas setas do teclado através da função updateControlledPlayer da Main.

Para que os fantasmas que estivessem no estado Dead andassem a metade da velocidade do Pacman, fizemos uma função em que quando o Step (número de iterações que o jogo já passou) é par, só jogam os Players que não são Dead Ghosts.
Os restantes Players não são afetados por essa restrição, tendo assim, o conceito de velocidade, sido implementado da maneira que se o Player for um Ghost Dead só jogará de 500 ms em 500 ms, enquanto que os outros jogam de 250 ms em 250 ms (DefaultDelayTime).
De seguida, para que haja a redução do TimeMega de um Pacman, foi introduzida uma função que a cada 4 iterações passadas reduziria em 1 o TimeMega, ou seja a cada segundo que passava era reduzido em 1 o valor do TimeMega dum Pacman Mega.
Consequente a essa redução de timeMega, alteramos certos aspetos da Tarefa2, modificando e criando novas funções que faria um Pacman no estado Mega e com MegaTime 0, voltar ao normal, e sem nenhum Pacman em modo Mega, transformar-se-iam de volta todos os fantasmas Dead, em Alive.

Para podermos verificar o bot da Tarefa 6, criamos a função jogaListab que, tal como na jogaLista (original), recebe as jogadas dos Ghosts para atualizar o State, mas o Pacman é controlado pelo bot da Tarefa 6 e não pelos inputs que realizamos através das setas do teclado.
Atualmente a função passTime está configurada para que o Pacman não seja controlado pelo bot, mas sim pelas setas do teclado.

Por fim, com todas estas novas funções em consideração a função final passTime foi criada, sendo assim concretizado o objetivo de calcular o efeito de passagem do tempo num State.

= Discussão e conclusão

Nesta tarefa foi atingido o seu principal objetivo, podendo ser verificada a passagem do tempo no mapa e nos jogadores através das funções criadas, havendo atualização do estado de jogo a cada iteração.

-}



defaultDelayTime = 250 -- 250 ms


-- | Dado uma lista de Players e um State, vai fazer um play para todos os jogadores utilizando as funções da seguinte tarefa, que analisam o State e determinam a melhor jogada a efetuar por parte dos players fantasmas, e para o Pacman, faz com que ele se mova no sentido da sua orientação.
jogaLista :: [Player] -> State -> State
jogaLista [] s = s
jogaLista p@(x:xs) s = let pac = thePacman p
                           jogadas = ghostPlayAux p s
                        in play (Move (getPlayerID pac) (getPlayerOrientation pac)) (plays jogadas s)


-- | Função que dado um conjunto de Plays e um State, aplica todas as Plays ao State.
plays :: [Play] -> State -> State 
plays [] s = s
plays (x:xs) s = plays xs (play x s)


-- | Dado uma lista de Players, retorna uma lista de Players, sem os Ghosts Dead.
arentDGhost :: [Player] -> [Player]
arentDGhost [] = []
arentDGhost ((Ghost (GhoState (a,b,c,d,e,f) Dead)):xs) = arentDGhost xs
arentDGhost (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)):xs) = p : arentDGhost xs
arentDGhost (p@(Pacman (PacState (a,b,c,d,e,f) g h i)):xs) = p : arentDGhost xs 


-- | Função que faz os players com metade da velocidade, fazerem uma jogada a cada duas iterações.
passTimeAux :: Int -> State -> State
passTimeAux n s@(State m x l) | even n = jogaLista (arentDGhost x) s
                              | otherwise = jogaLista x s 

-- | Dado uma lista de players devolve um lista de players com a redução em 1 segundo da duração do TimeMega, de todos os Pacmans que estão no estado Mega.
reduceMegaSec :: [Player] -> [Player]
reduceMegaSec [] = []
reduceMegaSec ((Pacman (PacState (a,b,c,d,e,f) g h Mega)):xs) = (Pacman (PacState (a,b,c,d,e,f) (g-1) h Mega)) : reduceMegaSec xs
reduceMegaSec (x:xs) = x : reduceMegaSec xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que reduz em um segundo a duração do TimeMega dos Pacman Mega.
reduceMega :: State -> State
reduceMega (State m x l) = (State m (reduceMegaSec x) l) 


-- | Função que diz se um número é multiplo de 4, utilizada para detetar a passagem de 1 segundo (=250*4 ms).
quadruplo :: Int -> Bool
quadruplo x | mod x 4 == 0 = True
            | otherwise = False


-- | Função final que aplica a passagem de tempo.
passTime :: Int -> State -> State
passTime n s | quadruplo n = reduceMega (passTimeAux n s) 
             | otherwise = passTimeAux n s




-- | Função utilizada para testar o bot da Tarefa 6, dando as jogadas dos fantasmas da Tarefa 5, mas com o Pacman a ser controlado pelo bot da Tarefa 6 
jogaListab :: [Player] -> State -> State
jogaListab [] s = s
jogaListab p@(x:xs) s = let pac = thePacman p
                            jogadas = ghostPlayAux p s
                        in play (transPlay (bot (getPlayerID pac) s )) (plays jogadas s)


-- | Transforma um Maybe Play num Play.
transPlay :: Maybe Play -> Play
transPlay (Just p) = p




-- | É uma função auxiliar que transforma um corredor de um labirinto, peça por peça, em uma lista de tuplos com o número 1 e a respetiva peça.
basicCorr :: Corridor       -- ^ Lista de peças que vão ser transformadas.
          -> [(Int, Piece)] -- ^ Lista de tuplos na forma [(1, /Piece/)], em que cada tuplo corresponde a uma peça.
basicCorr [] = []
basicCorr (x:xs) | x == Wall = ((1,Wall) : basicCorr xs)
                 | x == Food Little = ((1,Food Little) : basicCorr xs)
                 | x == Empty = ((1, Empty) : basicCorr xs)
                 | x == Food Big = ((1,Food Big) : basicCorr xs)


-- | Função que junta os números, respetivos a peças iguais consecutivas, numa lista de tuplos [(/Int/, /Piece/)], dando uma lista mais compacta de tuplos (/Int/, /Piece/).
gatherBasic :: [(Int, Piece)] -- ^ Lista de tuplos não otimizadamente compacta.
            -> [(Int, Piece)] -- ^ Lista de tuplos com os padrões horizontais compactados.
gatherBasic [x] = [x]
gatherBasic ((n,p):(n1,p1):xs) | p==p1 = gatherBasic ((n+n1,p):xs)
                               | otherwise = (n,p) : gatherBasic ((n1,p1):xs)


-- | Função que transforma, primeiramente, um corredor em uma lista do tipo (/Int/, /Piece/), de seguida dá-se a compactação dessa lista através da função gatherBasic enunciada anteriormente, transformando cada lista de tuplos compactada numa instrução do tipo Instruct (/Int/, /Piece/), para cada corredor do labirinto, dando no final um conjunto de instruções :: /Instructions/. 
basic :: Maze         -- ^ Conjunto de corredores que irão ser compactados num padrão horizontal.
      -> Instructions -- ^ Instruções do labirinto compactas num padrão horizontal.
basic [] = []
basic (x:xs) = [Instruct (gatherBasic (basicCorr x))] ++ basic xs 


-- | Função auxiliar que calcula a lista de posições em que um dado elemento ocorre numa lista.
elemIndices :: Eq a => a     -- ^ Elemento que queremos verificar se ocorre na lista.
                    -> [a]   -- ^ Lista onde queremos verificar a ocorrência do elemento.
                    -> [Int] -- ^ Lista de posições em que o elemento ocorre na lista.
elemIndices _ [] = []
elemIndices n (x:xs) | n==x = 0 : map (+1) (elemIndices n xs)
                     | otherwise = map (+1) (elemIndices n xs)


-- | Função auxiliar da função poeRepeats (função seguinte), que substitui a instrução (ordenada pelo inteiro recebido na 2ª entrada) numa lista de instruções, por uma instrução do tipo /Repeat Int/ ,cujo /Int/ é o fornecido no primeiro argumento. 
poeRepeatsaux :: Int          -- ^ Inteiro n, que vai estar na consituição da /Instruction/, /Repeat/ n, que vai subsituir uma determinada /Instruction/ numa lista.
              -> Int          -- ^ Indice da instrução que pretendemos substituir.
              -> Instructions -- ^ Lista de /Instruction/ que queremos alterar.
              -> Instructions -- ^ Lista de /Instruction/ com a substituição pretendida. 
poeRepeatsaux _ _ [] = [] 
poeRepeatsaux n p (x:xs) | p==0 = ((Repeat n):xs)
                         | otherwise = x : poeRepeatsaux n (p-1) xs


-- | Função que substitui as instruções (ordenadas pela lista de inteiros recebida no 2º argumento de entrada) de uma lista de instruções, por uma instrução do tipo /Repeat Int/, cujo /Int/ é o fornecido no primeiro argumento. 
poeRepeats :: Int          -- ^ Inteiro n, que vai estar na consituição da /Instruction/, /Repeat/ n, que vai subsituir certas /Instruction/ de uma lista.
           -> [Int]        -- ^ Indices das instruções que pretendemos subsituir.
           -> Instructions -- ^ Lista de /Instruction/ que queremos alterar.
           -> Instructions -- ^ Lista de /Instruction/ com a substituição desejada.
poeRepeats _ [] x = x
poeRepeats n (p:ps) x = poeRepeatsaux n p (poeRepeats n ps x)


-- | Função auxiliar à função seguinte, que recebe duas instruções, e que verifica se cada elemento da primeira /Instructions/, tem algum elemento igual à segunda /Instructions/, e no caso de haver, esses elementos repetidos (excetuando o primeiro) que estão na segunda lista, vão ser substituídos por uma instrução do tipo /Repeat/ n (em que n é o indice da primeira ocorrência desse elemento na primeira /Instructions/), através da funcão auxiliar anterior.
repeater :: Instructions -- ^ Primeira /Instructions/ em que vai ser verificado se cada elemento, tem algum igual à segunda /Instructions/.
         -> Instructions -- ^ Segunda /Instructions/, que vai sofrer as alterações caso hajam elementos da primeira /Instructions/ iguais a elementos desta segunda /Instructions/.
         -> Instructions -- ^ /Instructions/ com as alterações feitas à segunda /Instructions/.
repeater [] m = m
repeater (x:xs) m | elem x m = repeater xs (poeRepeats (head (elemIndices x m)) (tail(elemIndices x m)) m)
                  | otherwise = repeater xs m

-- | Função que utiliza a anterior, para verificar elementos repetidos num mesmo labirinto.
simpRepeater :: Instructions -- ^ /Instructions/ na qual vai ser verificado se existem elementos iguais ao longo da lista e caso haja, vão ser substituidos (excetuando o primeiro elemento dos repetidos) por uma /Instruction/ do tipo /Repeat Int/, em que o /Int/ é o indice do primeiro elemento dos repetidos.  
             -> Instructions -- ^ /Instructions/ final com os padrões verticais compactados.
simpRepeater m = repeater m m


-- | Função final que compacta ao máximo um labirinto em instruções.
compactMaze :: Maze         -- ^ Labirinto que irá ser compactado.
            -> Instructions -- ^ /Instructions/ compactadas que correspondem ao labirinto.
compactMaze m = simpRepeater (basic m)



-- * Funções-teste.

-- | Casos de labirintos para a função compactMaze.
m2 = [(generateMaze 15 10 43),(generateMaze 20 15 35),(generateMaze 17 11 4)]

-- | Testa os casos para a função compactMaze.
testItT3 :: [Maze] -> [Instructions]
testItT3 [] = []
testItT3 (x:xs) = compactMaze x : testItT3 xs


-- | Função auxiliar que faz o somatório do número de peças de uma lista de (/Int/, /Piece/).
somatorio :: [(Int,Piece)] -> Int
somatorio [] = 0
somatorio ((n,p):xs) = n + somatorio xs


-- | Testa se a Instruction é válida (ou seja, se o somatório do número de peças é igual à largura) para um determinado labirinto.
validInstruction :: Maze -> Instruction -> Bool
validInstruction m@(x:ms) (Instruct p) 
                   | length x == somatorio p = True
                   | otherwise = False
validInstruction m (Repeat n) = True


-- | Testa se as /Instructions/ são válidas para um determinado labirinto.
validInstructions :: Maze -> Instructions -> Bool
validInstructions m [] = True
validInstructions m (i:is) | validInstruction m i = validInstructions m is
                           | otherwise = False


-- | Descompacta uma instrução num corredor.
decCorridor :: Instruction -> Corridor
decCorridor (Instruct []) = [] 
decCorridor (Instruct ((n,p):xs)) | n==0 = decCorridor (Instruct xs)
                                  | otherwise = p : decCorridor (Instruct ((n-1,p):xs))


-- | Descompacta /Instructions/ básicas num labirinto.
decompact :: Instructions -> Maze
decompact [] = []
decompact (i:is) = decCorridor i : decompact is



-- * Funções de movimento do Pacman

-- | Faz o Pacman se mover, consoante a orientação dada e a orientação do Pacman.
movePlayer :: Player      -- ^ Um determinado /Player/.
           -> Orientation -- ^ A orientação dada.
           -> Player      -- ^ /Player/ com as devidas alterações na sua posição ou orientação.
movePlayer p Null = p
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo Dying)) d = Pacman (PacState (id,(x,y),v,or,pts,l) mg mo Dying)
movePlayer (Pacman (PacState (id,(x,y),v,L,pts,l) mg mo st)) L = Pacman (PacState (id,(x,y-1),v,L,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,R,pts,l) mg mo st)) R = Pacman (PacState (id,(x,y+1),v,R,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,U,pts,l) mg mo st)) U = Pacman (PacState (id,(x-1,y),v,U,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,D,pts,l) mg mo st)) D = Pacman (PacState (id,(x+1,y),v,D,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) L = Pacman (PacState (id,(x,y),v,L,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) R = Pacman (PacState (id,(x,y),v,R,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) U = Pacman (PacState (id,(x,y),v,U,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) D = Pacman (PacState (id,(x,y),v,D,pts,l) mg mo st)
movePlayer (Ghost (GhoState (id,(x,y),v,L,pts,l) st)) L = Ghost (GhoState (id,(x,y-1),v,L,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,R,pts,l) st)) R = Ghost (GhoState (id,(x,y+1),v,R,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,U,pts,l) st)) U = Ghost (GhoState (id,(x-1,y),v,U,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,D,pts,l) st)) D = Ghost (GhoState (id,(x+1,y),v,D,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) L = Ghost (GhoState (id,(x,y),v,L,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) R = Ghost (GhoState (id,(x,y),v,R,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) U = Ghost (GhoState (id,(x,y),v,U,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) D = Ghost (GhoState (id,(x,y),v,D,pts,l) st)



-- | Faz o Pacman passar pelo túnel, se estiver para entrar nele, caso contrário faz o seu movimento normal básico, através da função anterior.
moveInTunnel :: Maze        -- ^ Labirinto onde se encontra o /Player/.
             -> Player      -- ^ O /Player/ selecionado.
             -> Orientation -- ^ Orientação ordenada.
             -> Player      -- ^ O /Player/ selecionado com a respetiva mudança na sua posição ou orientação.
moveInTunnel (m:ms) p@(Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) dir
                            | y == 0 && dir == L && or == L = Pacman (PacState (id,(x,lgt),v,L,pts,l) mg mo st)
                            | y == lgt && dir == R && or == R = Pacman (PacState (id,(x,0),v,R,pts,l) mg mo st)
                            | otherwise = movePlayer p dir
                                        where lgt = (length m) - 1
moveInTunnel (m:ms) p@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)) dir
                            | y == 0 && dir == L && or == L = Ghost (GhoState (id,(x,lgt),v,L,pts,l) st)
                            | y == lgt && dir == R && or == R = Ghost (GhoState (id,(x,0),v,R,pts,l) st)
                            | otherwise = movePlayer p dir
                                        where lgt = (length m) - 1


-- | Função auxiliar que devolve o /Player/ ao qual é destinado o /Play/, ou seja, identifica o jogador com o ID da /Play/.
thePlayer :: Play     -- ^ Ordem /Play/ dirigida a um determinado /Player/ com uma certa ID. 
          -> [Player] -- ^ Lista de /Player/.
          -> Player   -- ^ /Player/ cuja /Play/ era dirigida, ou seja, o que tem o mesmo ID da /Play/.
thePlayer pl@(Move id or) (x:xs) | id == getPlayerID x = x
                                 | otherwise = thePlayer pl xs


-- | Função auxiliar que dá a peça que se encontra nas coordenadas do labirinto pedidas.
pieceInCoords :: Coords -- ^ Coordenadas do labirinto.
              -> Maze   -- ^ Respetivo labirinto. 
              -> Piece  -- ^ Peça que se encontra nas coordenadas do respetivo labirinto.
pieceInCoords (x,y) m = head $ drop y $ head $ drop x m


-- | Verifica se a posição para onde o Pacman vai, não tem uma peça /Wall/.
testWall :: Play  -- ^ Jogada. 
         -> State -- ^ Estado.
         -> Bool  -- ^ No caso de ter uma /Wall/ devolve /False/, caso contrário devolve /True/.
testWall (Move id Null) (State m x l) = True
testWall pl@(Move id or) (State m x l) | pieceInCoords (getPlayerCoords (moveInTunnel m (thePlayer pl x) or)) m == Wall = False
                                       | otherwise = True


-- | Dá uma lista de players com a alteração /Play/ destinada ao /Player/ com o respetivo ID.
moveID :: Play  -- ^ Jogada.
       -> State -- ^ Estado.
       -> State -- ^ Devolve um /State/ em que é feita a alteração no estado do respetivo /Player/ pretendida com o /Play/.
moveID (Move id or) (State m (x:xs) l) = (State m (map (\x -> if id==getPlayerID x then moveInTunnel m x or else x) (x:xs)) l)


-- | Função final que se aplicam todas as leis de movimento do Pacman.
movePac :: Play  -- ^ Jogada.
        -> State -- ^ Estado.
        -> State -- ^ Estado em que o /Player/ tem o seu estado alterado consoante a jogada, e que caso haja uma parede no caminho, ele não vá para ela.
movePac pl@(Move id or) s@(State m x l) | testWall pl s = moveID pl s
                                        | otherwise = State m x l


-- * Funções de atualização do score do Pacman

-- | Verifica se tem uma comida pequena nas coordenadas do Player com o ID do Play.
nextPieceSF :: Play  -- ^ Jogada.
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir uma comida pequena, caso contrário devolve False.
nextPieceSF (Move id or) (State m x l) | pieceInCoords (getPlayerCoords ((thePlayer (Move id or) x))) m == Food Little = True
                                       | otherwise = False

-- | Verifica se tem uma comida grande nas coordenadas do Player com o ID do Play.
nextPieceBF :: Play  -- ^ Jogada. 
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir uma comida grande, caso contrário devolve False.
nextPieceBF (Move id or) (State m x l) | pieceInCoords (getPlayerCoords ((thePlayer (Move id or) x))) m == Food Big = True
                                       | otherwise = False 

-- | Verifica se tem um fantasma morto nas coordenadas do Player com o ID do Play, através das coordenadas do Player e das coordenadas dos Ghosts Dead, verificando se a coordenada do Player coincide com a coordenada de algum Ghost, com a função "elem" pré-definida do Prelude.
nextPieceGh :: Play  -- ^ Jogada. 
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir um fantasma morto, caso contrário devolve False.
nextPieceGh (Move id or) (State m x l) | isPac (thePlayer (Move id or) x) && getPlayerID (thePlayer (Move id or) x) == id = elem (getPlayerCoords (thePlayer (Move id or) x)) (ghostsDCoords (State m x l))
                                       | otherwise = False


-- | Aumenta o score do Player com o ID do Play em 1 ponto.
score1ID :: Play     -- ^ Jogada com um determinado ID.
         -> [Player] -- ^ Conjunto de jogadores.
         -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +1 ponto.
score1ID pl [] = []
score1ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score1ID pl xs
score1ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs) 
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+1,l) mg mo st)) : xs
                                                         | otherwise = h : score1ID (Move id or) xs


-- | Aumenta o score do Player com o ID do Play em 5 pontos.
score5ID :: Play     -- ^ Jogada com um determinado ID.
         -> [Player] -- ^ Conjunto de jogadores.
         -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +5 pontos.
score5ID pl [] = []
score5ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score5ID pl xs
score5ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs)                                           
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+5,l) mg mo st)) : xs
                                                         | otherwise = h : score5ID (Move id or) xs                                                        


-- | Aumenta o score do Player com o ID do Play em 10 pontos.
score10ID :: Play     -- ^ Jogada com um determinado ID.
          -> [Player] -- ^ Conjunto de jogadores.
          -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +10 pontos.
score10ID pl [] = []
score10ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score10ID pl xs
score10ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs)
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+10,l) mg mo st)) : xs
                                                         | otherwise = h : score10ID (Move id or) xs


-- | Atualiza os pontos do State após uma jogada, verificando o que ele come para de seguida atribuir a respetiva pontuação a esse Pacman através das funções auxiliares anteriores.
makeScore :: Play  -- ^ Jogada.
          -> State -- ^ Estado.
          -> State -- ^ Estado atualizado, relativamente ao Score, tendo em conta o que o Pacman comeu.
makeScore pl@(Move id or) s@(State m x l) | nextPieceSF pl s && nextPieceGh pl s = State m (score1ID pl (score10ID pl x)) l
                                          | nextPieceBF pl s && nextPieceGh pl s = State m (score5ID pl (score10ID pl x)) l
                                          | nextPieceSF pl s = State m (score1ID pl x) l
                                          | nextPieceBF pl s = State m (score5ID pl x) l
                                          | nextPieceGh pl s = State m (score10ID pl x) l
                                          | otherwise = s



-- * Função de remoção de peça do labirinto

-- | Retira a comida do labirinto, após ser comida pelo Pacman através da função no ficheiro Types, que dado uma coordena, uma peça, e um labirinto, ela substitui a peça na coordenada pela peça pedida, nesse determinado labirinto. Para remover a peça comida damos a coordenada do Player, a peça Empty e o labirinto do State, para assim remover a peça.
clearFood :: Play  -- ^ Jogada.
          -> State -- ^ Estado.
          -> State -- ^ Estado atualizado relativamente à remoção de peças do labirinto, comidas pelo Pacman.
clearFood pl@(Move id or) s@(State m x l) 
                        | (nextPieceSF pl s || nextPieceBF pl s) && isPac (idPlayer x id) = State (replaceElemInMaze (getPlayerCoords (thePlayer pl x)) Empty m) x l
                        | otherwise = s



-- * Funções que controlam o nº de vidas do Pacman, bem como a sua atualização para o estado Dying.

-- | Verifica se tem um fantasma vivo nas coordenadas do Player com o ID do Play, através das coordenadas do Player e das coordenadas dos Ghosts Alive, verificando se a coordenada do Player coincide com a coordenada de algum Ghost com a função pré-definida "elem" do Prelude.
isAliveGhost :: Play  -- ^ Jogada.
             -> State -- ^ Estado.
             -> Bool  -- ^ Devolve True se algum fantasma vivo, está nas coordenadas do Pacman, caso contrário devolve False.
isAliveGhost (Move id or) (State m x l) = elem (getPlayerCoords (thePlayer (Move id or) x)) (ghostsAlCoords (State m x l))


-- | Função auxiliar que tira uma vida a um Player ou que passa o Pacman para Dying se não tiver vidas restantes. 
takeJgdrLife :: Play     -- ^ Jogada.
             -> [Player] -- ^ Conjunto de jogadores.
             -> [Player] -- ^ Conjunto de jogadores, em que é retirado uma vida ou passa a Dying se não tiver mais nenhuma, ao /Player/ com a mesma ID da /Play/.
takeJgdrLife pl [] = []
takeJgdrLife pl (a@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)):xs) = a : takeJgdrLife pl xs            
takeJgdrLife pl@(Move id or) (p@(Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo st)):xs)
                                                                         | id == n0 && getPlayerLives p > 0 = (Pacman (PacState (n0,(x,y),v,d,pts,l-1) mg mo st):xs)
                                                                         | id == n0 && getPlayerLives p == 0 = (Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo Dying):xs)
                                                                         | otherwise = p : takeJgdrLife pl xs


-- | Retira uma vida ao Pacman ou passa o Pacman a Dying se não tiver vidas restantes, quando o Pacman entra em contacto com um Ghost Alive.
takeLife :: Play  -- ^ Jogada.
         -> State -- ^ Estado.
         -> State -- ^ Estado em que se no seu [/Player/] houver algum Ghost vivo nas coordenadas do Pacman, então é retirado uma vida ou passa a Dying se não tiver vidas restantes.
takeLife pl@(Move id or) s@(State m x lv) | isAliveGhost pl s = State m (takeJgdrLife pl x) lv
                                          | otherwise = s



-- * Funções de transformação de estado dos Players

-- | Dado uma Play e um conjunto de players, torna o Pacman com o ID da Play em Mega.
turnToMega :: Play     -- ^ Jogada com um determinado ID.
           -> [Player] -- ^ Conjunto de jogadores.
           -> [Player] -- ^ Conjunto de jogadores em que o /Player/ com o ID da /Play/, passa a Mega.
turnToMega pl [] = []
turnToMega pl (a@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)):xs) = a : turnToMega pl xs
turnToMega pl@(Move id or) (p@(Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo st)):xs)
                                                  | id == n0 = (Pacman (PacState (n0,(x,y),v,d,pts,l) 10 mo Mega):xs)
                                                  | otherwise = p : turnToMega pl xs


-- | Função auxiliar para obter a orientação oposta, necessária para a Tarefa 5.
oposta :: Orientation -> Orientation
oposta Null = L
oposta L = R
oposta R = L
oposta D = U
oposta U = D


-- | Função auxiliar que transforma um conjunto de fantasmas vivos em fantasmas mortos e dando a orientação oposta à que tinham (a pedido da Tarefa 5), não fazendo alterações aos Players que não são fantasmas mortos.
transGhDead :: [Player] -- ^ Conjunto de jogadores.
            -> [Player] -- ^ Conjunto de jogadores em que os fantasmas que estavam vivos passaram a mortos.
transGhDead [] = []
transGhDead ((Ghost (GhoState (id,(x,y),v,or,pts,l) Alive)):xs) = Ghost (GhoState (id,(x,y),(v/2),(oposta or),pts,l) Dead) : transGhDead xs
transGhDead (x:xs) = x : transGhDead xs


-- | Transforma o Pacman em Mega (caso coma Big Food), consequentemente transformando todos os fantasmas vivos em mortos.
makeMega :: Play  -- ^ Jogada.
         -> State -- ^ Estado.
         -> State -- ^ Estado em que se o Pacman entrou em contacto com uma /Big Food/, então transforma-se em Mega e transforma todos os fantasmas vivos do [/Player/] em fantasmas mortos. Caso contrário, não há alterações relativamente ao PacMode do Pacman nem ao estado dos fantasmas.
makeMega pl@(Move id or) s@(State m x l) | nextPieceBF pl s && isPac (idPlayer x id) = State m (transGhDead (turnToMega pl x)) l
                                         | otherwise = s 



-- * Funções de teleporte dos fantasmas para a sua casa

-- | Função auxiliar que manda um Ghost comido pelo Pacman para a casa dos fantasmas, retornando o seu estado para Alive e a sua velocidade para o normal (multiplicando por 2).
resetSpawn :: Coords   -- ^ Coordenadas pedidas.
           -> State    -- ^ Estado com um determinado labirinto.
           -> [Player] -- ^ Conjunto de jogadores em que se um /Ghost Dead/ estiver nas coordenadas pedidas, ele é mandado para as coordenadas obtidas através da função auxiliar anterior (que o mandam para a casa dos fantasmas) e o seu estado volta a ser Normal. 
resetSpawn c (State m [] l) = [] 
resetSpawn c (State m (h@(Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)):xs) lv) = h : resetSpawn c (State m xs lv)
resetSpawn c (State m (h@(Ghost (GhoState (id,(x,y),v,or,pts,l) Alive)):xs) lv) = h : resetSpawn c (State m xs lv)
resetSpawn c (State m (h@(Ghost (GhoState (id,(x,y),v,or,pts,l) Dead)):xs) lv)
                                          | c == (x,y) = (Ghost (GhoState (id,midCoord m,v*2,or,pts,l) Alive)):resetSpawn c (State m xs lv)
                                          | otherwise = h : resetSpawn c (State m xs lv)


-- | Dado um /Play/ e um /State/, dá um /State/ com a alteração nas coordenadas e no estado do /Ghost/ comido caso haja algum Pacman que tenha comido um /Ghost Dead/, caso contrário não há alterações nas coordenadas nem no estado de nenhum /Ghost/.
goHome :: Play  -- ^ Jogada com um determinado ID.
       -> State -- ^ Estado.
       -> State -- ^ Estado em que se o Pacman com o determinado ID, se encontra nas mesmas coordenadas de um /Ghost Dead/, então esse /Ghost/ é enviado para a casa dos fantasmas e o seu estado e velocidade voltam ao normal.
goHome pl@(Move id or) s@(State m x l) | nextPieceGh pl s = State m (resetSpawn (getPlayerCoords(thePlayer pl x)) s) l
                                       | otherwise = s


-- * Função final

-- | Função final com todas as ações que o Pacman tem de realizar no decurso de uma jogada.
play :: Play  -- ^ Jogada.
     -> State -- ^ Estado. 
     -> State -- ^ Estado final em que dependendo da jogada realizada, o estado é atualizado, tendo em conta todas as ações a realizar.
play p@(Move id or) s@(State m x l) = checkMega $ checkTimeMg $ moveMouth $ goHome p $ takeLife p $ makeMega p $ makeScore p $ movePac p $ clearFood p s



-- * Funções complementares para a 2ª fase

-- | Fecha a boca aos Pacmans com boca aberta e vice-versa numa lista de jogadores. (talvez meter um matching para caso o pacman esteja dying)
ismouth :: [Player] -> [Player]
ismouth [] = []
ismouth ((Pacman (PacState (x,y,z,t,h,l) b Open d)):xs) = (Pacman (PacState (x,y,z,t,h,l) b Closed d)) : ismouth xs
ismouth ((Pacman (PacState (x,y,z,t,h,l) b Closed d)):xs) = (Pacman (PacState (x,y,z,t,h,l) b Open d)) : ismouth xs
ismouth (x:xs) = x : ismouth xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior de fechar e abrir a boca aos Pacmans, consoante o necessário. 
moveMouth :: State -> State
moveMouth (State m x l) = State m (ismouth x) l



-- | Verifica se há algum Pacman no modo Mega.
areMega :: [Player] -> Bool
areMega [] = False
areMega ((Pacman (PacState (x,y,z,t,h,l) b c Mega)):xs) = True
areMega (x:xs) = areMega xs

-- | Transforma todos os fantasmas mortos em fantasmas vivos.
turnToAlive :: [Player] -> [Player]
turnToAlive [] = []
turnToAlive ((Ghost (GhoState (x,y,z,t,h,l) Dead)):xs) = (Ghost (GhoState (x,y,z,t,h,l) Alive)) : turnToAlive xs
turnToAlive (x:xs) = x : turnToAlive xs

-- | Transforma os fantasmas mortos em vivos se não houver nenhum Pacman no modo Mega.
checkMegaPl :: [Player] -> [Player]
checkMegaPl [] = []
checkMegaPl x | areMega x = x
              | otherwise = turnToAlive x

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que transforma os fantasmas mortos em fantasmas vivos se não houver nenhum Pacman em modo Mega.
checkMega :: State -> State
checkMega (State m x l) = State m (checkMegaPl x) l 



-- | Verifica se um Pacman tem MegaTime <= 0 estando em modo Mega, transformando-o para o estado Normal nesse caso. (TALVEZ METER AQUI O TIRAR O TEMPO DO MEGATIME POR PLAY)
checkTimeAux :: [Player] -> [Player]
checkTimeAux [] = []
checkTimeAux (p@(Pacman (PacState (x,y,z,t,h,l) b c Mega)):xs) | b <= 0 = (Pacman (PacState (x,y,z,t,h,l) 0 c Normal)) : checkTimeAux xs
                                                               | otherwise = p : checkTimeAux xs
checkTimeAux (x:xs) = x : checkTimeAux xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que transforma um Pacman para o estado Normal se um Pacman tem MegaTime <= 0 estando em modo Mega.
checkTimeMg :: State -> State
checkTimeMg (State m x l) = State m (checkTimeAux x) l



-- * Funções-teste.
-- | Exemplo de labirinto.
m1 :: Maze
m1 = generateMaze 15 10 54

-- | Casos de Plays e States para a função play.
testCasesPlay :: [(Play,State)]
testCasesPlay = [((Move 0 R),State m1 [(Pacman (PacState (0,(1,9),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,3),2,L,0,2) 0 Open Normal))] 1),((Move 0 L), State m1 [(Pacman (PacState (0,(1,9),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,7),2,L,0,2) 0 Closed Normal))] 2), ((Move 0 R), State m1 [(Pacman (PacState (0,(2,8),2,R,0,2) 10 Open Mega)),(Ghost (GhoState (1,(2,9),2,L,0,2) Dead))] 1), ((Move 0 R),State m1 [(Pacman (PacState (0,(5,14),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,3),2,L,0,2) 0 Open Normal))] 1)]

-- | Testa os casos para a função play.
testItT2 :: [(Play,State)] -> [State]
testItT2 [] = []
testItT2 ((a,b):xs) = play a b : testItT2 xs


-- | Testa se algum /Player/ de um /State/, se encontra indevidamente nas coordenadas de uma /Wall/, dando /True/ caso haja.
isOnWall :: State -> Bool
isOnWall (State m [] l) = False
isOnWall (State m (p:ps) l) | pieceInCoords (getPlayerCoords p) m == Wall = True
                            | otherwise = isOnWall (State m ps l)


-- | Dada uma /seed/, retorna a lista dos n inteiros gerados aleatoriamente.
generateRandoms :: Int   -- ^ Número de inteiros a ser gerado.
                -> Int   -- ^ /seed/ de números aleatórios.
                -> [Int] -- ^ Lista dos números aleatórios. 
generateRandoms n seed = let gen = mkStdGen seed -- Cria um gerador aleatório.
                         in take n $ randomRs (0,99) gen -- Tira os primeiros n elementos de uma série infinita de números aleatórios entre 0 e 99.


-- | Converte uma lista numa lista de listas de tamanho n.
subList :: Int   -- ^ Tamanho n das sublistas.
        -> [a]   -- ^ Lista que vai ser dividida.
        -> [[a]] -- ^ Lista de listas de tamanho n resultante da divisão da lista pedida.
subList _ [] = []
subList n l = take n l : subList n (drop n l)


-- | Converte um número inteiro numa /Piece/.
convertPiece :: Int   -- ^ Inteiro entre 0 e 99 que vai ser convertido.
             -> Piece -- ^ Peça resultante da conversão.
convertPiece x
        | x == 30 = Food Big
        | x >= 0 && x<70 = Food Little
        | otherwise = Wall


-- | Converte um corredor numa /String/.
printCorridor :: Corridor -- ^ Conjunto de peças.
              -> String   -- ^ /String/ em que está representado simbolicamente o corredor.
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs


-- | Converte um labirinto numa /String/.
mazeToString :: Maze   -- ^ Conjunto de corredores.
             -> String -- ^ /String/ em que está representado simbolicamente o labirinto.
mazeToString [] = []
mazeToString (x:xs) = printCorridor x ++ mazeToString xs


-- | Converte uma lista de inteiros num corredor.
convertCorridor :: [Int]    -- ^ Lista de inteiros que irão ser convertidos em peças.
                -> Corridor -- ^ Lista de peças convertidas.
convertCorridor [] = []
convertCorridor (x:xs) = convertPiece x : convertCorridor xs


-- | Converte uma lista de inteiros num labirinto.
convertMaze :: [[Int]] -- ^ Lista de listas de inteiros que irão ser convertidas em corredores.
            -> Maze    -- ^ Lista de corredores convertidos.
convertMaze [] = []
convertMaze (x:xs) = convertCorridor x : convertMaze xs


-- | Pede um número e constrói uma lista de paredes com esse tamanho.
justWalls :: Int      -- ^ Tamanho da lista pretendido.
          -> Corridor -- ^ Lista com tamanho n só com paredes.
justWalls 0 = []
justWalls n = Wall : justWalls (n-1)


-- | Substitui os limites superior e inferior do labirinto por paredes.
wallsUpDown :: Maze -- ^ Labirinto que vai sofrer as alterações. 
            -> Maze -- ^ Labirinto com paredes em cima e em baixo.
wallsUpDown (x:xs) = justWalls (length x) : init xs ++ [justWalls (length x)]


-- | Substitui os limites laterais do labirinto por paredes.
sideWalls :: Maze -- ^ Labirinto que vai sofrer as alterações.
          -> Maze -- ^ Labirinto com os limites laterais substituídos por paredes.
sideWalls [] = []
sideWalls (x:xs) = (Wall : (init (tail x)) ++ [Wall]) : sideWalls xs


-- | Substitui todos os limites do labirinto por paredes, utilizando as funções auxiliares anteriores.
closeMaze :: Maze -- ^ Labirinto que vai sofrer as alterações.
          -> Maze -- ^ Labirinto cercado por paredes.
closeMaze x = wallsUpDown (sideWalls x)


-- | Dá o número da posição do corredor central do labirinto.
getMiddle :: Maze -> Int
getMiddle x = (div (length x) 2)


-- | Substitui o primeiro e último elemento de um corredor por uma peça.
replaceInCorridorAB :: Maze  -- ^ Labirinto que vai sofrer as alterações.
                    -> Int   -- ^ Indice do corredor que vai ser alterado.
                    -> Piece -- ^ Peça pela qual queremos substituir.
                    -> Maze  -- ^ Labirinto com o primeiro e último elemento de um corredor substituído por uma determinada peça.
replaceInCorridorAB [] y p = []
replaceInCorridorAB (x:xs) 0 p = (replaceInList (length x - 1) p (replaceInList 0 p x)) : xs
replaceInCorridorAB (x:xs) y p = x : replaceInCorridorAB xs (y - 1) p


-- | Substitui num corredor o Indice 0 i (i :: /Int/), por uma determinada peça. 
replaceInList :: Int      -- ^ Indice no corredor que quer subsituir.
              -> Piece    -- ^ Peça pela qual quer substituir.
              -> Corridor -- ^ Corredor que vai sofrer as alterações.
              -> Corridor -- ^ Corredor com os indices 0 i substituídos por uma determinada peça. 
replaceInList i p [] = []
replaceInList 0 p (x:xs) = p : xs
replaceInList i p (x:xs) = x : (replaceInList (i-1) p xs)


-- | Faz o túnel no labirinto tendo em conta a sua altura, utilizando as funções auxiliares anteriores.
makeTunnel :: Maze -- ^ Labirinto em que vai ser construído o túnel.
           -> Maze -- ^ Labirinto com o túnel implementado.
makeTunnel x | odd (length x) = replaceInCorridorAB x mid Empty
             | otherwise = (replaceInCorridorAB (replaceInCorridorAB x mid Empty) (mid - 1) Empty)
                 where mid = getMiddle x 


-- | Função conjunta que é aplicada no generateMaze, que em primeiro fecha o labirinto e de seguida constrói o túnel.
mazeTunel :: Maze -- ^ Labirinto que vai ser fechado por paredes, sendo de seguida implementado o túnel.
          -> Maze -- ^ Labirinto rodeado por paredes mas com o túnel.
mazeTunel x = makeTunnel (closeMaze x)


-- | Função que nos dá o número da posicão da peça central dum corredor.
midCorridor :: Corridor -> Int
midCorridor x = div (length x) 2


-- | Constituição da parte central do labirinto que irá conter a casa dos fantasmas, tendo em conta as dimensões do labirinto.
constroiCasa :: Maze -- ^ Labirinto cujas dimensões vão ser analisadas.
             -> Maze -- ^ Constituição parte central que contém a casa dos fantasmas, relativamente ao labirinto pedido.
constroiCasa a@(x:xs)
   | even (length x) =  [
              take (midC - 5) midL0 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 5) midL0,
              take (midC - 5) midL1 ++ [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty] ++ drop (midC + 5) midL1,
              take (midC - 5) midL2 ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ drop (midC + 5) midL2,
              take (midC - 5) midL3 ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty] ++ drop (midC + 5) midL3,
              take (midC - 5) midL4 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 5) midL4      
                        ]

   | otherwise =  [  
               take (midC - 5) midL0 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 6) midL0,
               take (midC - 5) midL1 ++ [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty] ++ drop (midC + 6) midL1,
               take (midC - 5) midL2 ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ drop (midC + 6) midL2,
               take (midC - 5) midL3 ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty] ++ drop (midC + 6) midL3,
               take (midC - 5) midL4 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 6) midL4 
                  ]
                        where midC = midCorridor x
                              midL0 = head (drop (getMiddle a - 2) a) -- primeiro elemento da parte central do labirinto.
                              midL1 = head (drop (getMiddle a - 1) a) -- segundo elemento da parte central do labirinto.
                              midL2 = head (drop (getMiddle a) a)     -- terceiro elemento da parte central do labirinto.
                              midL3 = head (drop (getMiddle a + 1) a) -- quarto elemento da parte central do labirinto.
                              midL4 = head (drop (getMiddle a + 2) a) -- quinto elemento da parte central do labirinto.


-- | Constrói a casa dos fantasmas no labirinto, tendo em conta a paridade da sua altura.
ghostHouse :: Maze -- ^ Labirinto no qual irá ser implementada a casa dos fantasmas.
           -> Maze -- ^ Labirinto com a casa dos fantasmas construída.
ghostHouse a | even (length a) = take (mid - 3) a ++ constroiCasa a ++ drop (mid + 2) a 
             | otherwise = take (mid - 2) a ++ constroiCasa a ++ drop (mid + 3) a
                  where mid = getMiddle a


-- | Função auxiliar para avaliar a validade do labirinto quanto às suas dimensões.
validM :: Int  -- ^ Largura do labirinto. 
       -> Int  -- ^ Altura do labirinto.
       -> Bool -- ^ /Bool/ que nos diz se o labirinto cumpre os requisitos mínimos das suas dimensões.
validM x y | x >= 15 && y >= 10 = True
           | otherwise = False


-- | Pede as dimensões do labirinto e a sua seed e faz um labirinto válido, dando erro caso as suas dimensões não cumpram os requisitos mínimos.
generateMaze :: Int  -- ^ Largura do labirinto.
             -> Int  -- ^ Altura do labirinto.
             -> Int  -- ^ /seed/ do labirinto.
             -> Maze -- ^ Labirinto válido com todos os requisitos estruturais e com as dimensões mínimas de 15x10.
generateMaze x y z | validM x y = mazeTunel (ghostHouse (convertMaze (subList x (generateRandoms (x*y) z))))
                   | otherwise = error "O labirinto não cumpre os requisitos mínimos para ser gerado (l>=15 e h>=10)"



-- | Mostra cada peça do labirinto nos seus respetivos símbolos atribuidos para uma visualização mais gráfica do labirinto.
imprimeMaze :: Maze -> IO ()
imprimeMaze l = do putStrLn ( mazeToString ( l ))


-- | Dado a sua largura, altura e uma /seed/, a função mostra o labirinto com essas características, de uma maneira mais gráfica para uma melhor visualização. 
genTest :: Int -> Int -> Int -> IO ()
genTest x y z = let gen = generateRandoms (x*y) z
                in imprimeMaze (mazeTunel (ghostHouse (convertMaze (subList x gen))))



-- * Funções-teste
-- | Casos de argumentos para a função generateMaze.
testCasesGenerateMaze :: [(Int,Int,Int)]
testCasesGenerateMaze = [(15,10,23),(20,10,45),(21,11,987),(16,13,765),(10,4,43),(50,50,50)]

-- | Testa os casos para a função generateMaze.
testItT1 :: [(Int,Int,Int)] -> [Maze]
testItT1 [] = []
testItT1 ((a,b,c):xs) = generateMaze a b c : testItT1 xs



-- | Verifica se a largura e comprimento do labirinto gerado cumprem os requisitos mínimos.
validMeasures :: Maze -> Bool
validMeasures m@(x:xs) | length x >= 15 && length m >= 10 = True
                       | otherwise = False


-- | Função auxiliar que verifica se um corredor tem só peças Wall.
checkCorr :: Corridor -> Bool
checkCorr [] = True
checkCorr c@(x:xs) | x==Wall = checkCorr xs
                   | otherwise = False


-- | Verifica se o primeiro e o último corredor estão com uma lista de Wall.
checkWallAB :: Maze -> Bool
checkWallAB [x] = checkCorr x
checkWallAB m@(x:xs) | checkCorr x = checkWallAB [last m]
                     | otherwise = False



data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )



-- | Configura a orientação de um /Player/ de acordo com a orientação dada.
setPlayerOrientation :: Player      -- ^ O /Player/ selecionado.
                     -> Orientation -- ^ A orientação desejada.
                     -> Player      -- ^ O /Player/ com a orientação desejada.
setPlayerOrientation (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) o = (Pacman (PacState (id,(x,y),v,o,pts,l) mg mo st))
setPlayerOrientation (Ghost (GhoState (id,(x,y),v,or,pts,l) gm)) o = (Ghost (GhoState (id,(x,y),v,o,pts,l) gm))


changeOrID :: Int -> Orientation -> [Player] -> [Player]
changeOrID _ _ [] = []
changeOrID n or (x:xs) | getPlayerID x == n = (setPlayerOrientation x or) : xs
                       | otherwise = x : changeOrID n or xs


changeOrientation :: Orientation -> Manager -> Manager
changeOrientation ori (Manager (State m x l) id b c d e) = (Manager (State m (changeOrID id ori x) l) id b c d e)


updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyUpArrow    m = changeOrientation U m 
updateControlledPlayer KeyDownArrow  m = changeOrientation D m
updateControlledPlayer KeyLeftArrow  m = changeOrientation L m
updateControlledPlayer KeyRightArrow m = changeOrientation R m
updateControlledPlayer k m = m


updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state pid step before delta delay) = (Manager state pid step now (delta+(now-before)) delay)

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state pid step before delta delay) = (Manager state pid step now 0 delay)

nextFrame :: Integer -> Manager -> Manager
nextFrame now man = let update = (resetTimer now man)
                    in update  { state = (passTime (step man) (state man)) , step = (step man) + 1}



loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorDefault  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager



loadMaze :: String -> State
loadMaze filepath = unsafePerformIO $ readStateFromFile filepath


readStateFromFile :: String -> IO State
readStateFromFile f = do
                content <- readFile f
                let llines = lines content 
                    (new_map,pl) = convertLinesToPiece llines 0 []
                return (State new_map pl 1)



convertLinesToPiece :: [String] -> Int -> [Player] -> (Maze,[Player])
convertLinesToPiece [] _ l = ([],l)
convertLinesToPiece (x:xs) n l  = let (a,b) = convertLineToPiece x n 0 l 
                                      (a1,b1) = convertLinesToPiece xs (n+1) b
                                  in (a:a1,b1)

convertLineToPiece :: String -> Int -> Int -> [Player] -> ([Piece],[Player])
convertLineToPiece [] _ _ l = ([],l)
convertLineToPiece (z:zs) x y l = let (a,b ) = charToPiece z x y l
                                      (a1,b1) = convertLineToPiece zs x (y+1) b 
                                  in (a:a1,b1)


charToPiece :: Char -> Int -> Int -> [Player] -> (Piece,[Player])
charToPiece c x y l
    | c == '{' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Normal ))   :l) )
    | c == '<' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Closed Normal )) :l) )
    | c == '}' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Open Normal ))   :l) )
    | c == '>' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Closed Normal )) :l) )
    | c == 'V' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Open Normal ))   :l) )
    | c == 'v' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Closed Normal )) :l) )
    | c == '^' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Open Normal ))   :l) )
    | c == '|' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Closed Normal )) :l) )
    | c == 'X' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Dying ))    :l) )
    | c == 'M' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Alive))            :l) )
    | c == '?' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Dead))             :l) )
    | c == 'o' =  (Food Big,l)
    | c == '.' =  (Food Little,l)
    | c == '#' =  (Wall,l)
    | otherwise = (Empty,l)
-- | Este módulo define os tipos de dados comuns a todos os alunos, tal como descrito e utilizado no 


-- * Tipos de dados auxiliares

-- | Estado do jogo
data State = State 
    {
        maze :: Maze -- ^ Labirinto 
    ,   playersState :: [Player] -- ^ Lista de jogadores
    ,   level :: Int -- ^ Níveç
    }

-- | Labirinto 
type Maze = [Corridor]

-- | Corredor
type Corridor = [Piece]

-- | Peça
data Piece =  Food FoodType -- ^ Comida
            | PacPlayer Player -- ^ Jogador
            | Empty -- ^ Peça Empty
            | Wall -- ^ Parede
            deriving (Eq)


-- | Jogador
data Player =  Pacman PacState -- ^ Pacman
             | Ghost GhoState -- ^ Fantasma
             deriving (Eq) 

-- | Orientação
data Orientation = L -- ^ Esquerda
                 | R -- ^ Direita 
                 | U -- ^ Cima 
                 | D -- ^ Baixo
                 | Null -- ^ Null
                 deriving (Eq,Show)

-- | Estado do Pacman
data PacState= PacState 
    {   
        pacState :: PlayerState -- ^ Estado do joador
    ,   timeMega :: Double -- ^ Tempo no modo Mega
    ,   openClosed :: Mouth -- ^ Estado da boca
    ,   pacmanMode :: PacMode -- ^ Modo
    
    } deriving Eq

-- | Estado do fantasma
data GhoState= GhoState 
    {
        ghostState :: PlayerState -- ^ Estado do jogador
    ,   ghostMode :: GhostMode -- ^ Modo
    } deriving Eq

-- | Coordenadas 
type Coords = (Int,Int)

-- | Estado do jogador
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 

-- | Estado da boca
data Mouth = Open -- ^ Aberta
           | Closed -- ^ Fechada
           deriving (Eq,Show)

-- | Modo do Pacman
data PacMode = Dying -- ^ A morrer
             | Mega -- ^ Mega
             | Normal -- ^ Normal
             deriving (Eq,Show)

-- | Estado do fantasma
data GhostMode = Dead -- ^ Morto
               | Alive -- ^ Vivo
               deriving (Eq,Show)

-- | Tipo de comida
data FoodType = Big -- ^ Grande
              | Little  -- ^ Pequena
              deriving (Eq)

-- | Cor
data Color = Blue -- ^ Azul
           | Green -- ^ Verde
           | Purple -- ^ Roxo
           | Red -- ^ Vermelho 
           | Yellow -- ^ Amarelo
           | None -- ^ None
           deriving Eq 

-- | Jogada
data Play = Move
            Int -- ^ ID do jogador
            Orientation -- ^ Orientação
            deriving (Eq,Show)

-- | Instruções
type Instructions = [Instruction]

-- | Instrução
data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)

-- | Manager
data Manager = Manager 
    {   
         state :: State -- ^ Estado
    ,    pid    :: Int -- ^ ID do jogador
    ,    step :: Int -- ^ Step
    ,    before :: Integer -- ^ Instante de tempo de tempo em que foi efetuada a última jogada
    ,    delta :: Integer -- ^ Tempo decorrido em milissegundos desde a última jogada
    ,    delay :: Integer -- ^ Intervalo de tempo entre jogadas
    } 

instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) =  "#" 
   show (  Empty ) =  " " 
   show (  Food z ) = show z
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) =  (show ( PacState (i, c, x, y,z,l) o m Normal)  ) 
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) =  (show ( PacState (i, c, x, y,z,l) o m Mega)  ) 
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) =  (show ( PacState (i, c, x, y,z,l) o m Dying)  ) 
   show ( PacPlayer (Ghost z) ) = (show z) 

{-
está em comentário para evitar conflito com o Main
coloredString :: String -> Color -> String
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x
-}

-- | Função que coloca os jogadores no labirinto
placePlayersOnMap :: [Player] -- ^ Lista de jogadores
                  -> Maze -- ^ Labirinto
                  -> Maze -- ^ Labirinto atualizado
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )

-- | Função que imprime um labirinto 
printMaze :: Maze -- ^ Labirinto 
          -> String -- ^ Representação textual do labirinto
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

-- | Função que imprime a informação relativa a um jogador
printPlayerStats :: Player -- ^ Jogador
                 -> String -- ^ Representação textual do jogador
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

-- | Função que devolve o ID de um jogador
getPlayerID :: Player -- ^ Jogador 
            -> Int -- ^ ID do jogador
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
-- | Função que devolve o nº de pontos de um jogador
getPlayerPoints :: Player -- ^ Jogador
                -> Int -- ^ Nº de pontos
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

-- | Função que atualiza o número de pontos de um jogador
setPlayerPoints :: Int -- ^ Pontos
                -> Player -- ^ Jogador
                -> Player -- ^ Jogador com os pontos atualizados
setPlayerPoints h' (Pacman (PacState (x,y,z,t,h,l) q c d )) = (Pacman (PacState (x,y,z,t,h',l) q c d ))
setPlayerPoints h' (Ghost (GhoState (x,y,z,t,h,l) q )) = (Ghost (GhoState (x,y,z,t,h',l) q ))

-- | Função que atualiza as coordenadas do jogador
setPlayerCoords :: Player -- ^ Jogador
                -> Coords -- ^ Coordenadas 
                -> Player -- ^ Jogador com as coordenadas atualizadas
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )

-- | Função que determina a orientação de um jogador
getPieceOrientation :: Piece -- ^ Peça
                    -> Orientation -- ^ Orientação
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

-- | Função que devolve o modo de um jogador do tipo Pacman
getPacmanMode :: Player -- ^ Jogador
              -> PacMode -- ^ Modo 
getPacmanMode (Pacman (PacState a b c d)) = d

-- | Função que altera o estado de um Pacman
setPacmanMode :: PacMode -- ^ Estado
              -> Player -- ^ Pacman
              -> Player -- ^ Pacman com o estado atualizado
setPacmanMode d' (Pacman (PacState a b c d)) = Pacman (PacState a b c d')

-- | Função que devolve o estado do jogador
getPlayerState :: Player -- ^ Jogador
               -> PlayerState -- ^ Estado do jogador
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

-- | Função que devolve a orientação do jogador
getPlayerOrientation :: Player -- ^ Jogador
                     -> Orientation -- ^ Orientação
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

-- | Função que altera a orientação do jogador
setPlayerOrientation :: Orientation -- ^ Orientação
                     -> Player -- ^ Jogador 
                     -> Player -- ^ Jogador com a orientação atualizada
setPlayerOrientation o (Pacman (PacState (x,y,z,t,h,l) q c d )) = (Pacman (PacState (x,y,z,o,h,l) q c d ))
setPlayerOrientation o (Ghost (GhoState (x,y,z,t,h,l) q )) = (Ghost (GhoState (x,y,z,o,h,l) q ))

-- | Função que substitui uma peça no labirinto
replaceElemInMaze :: Coords -- ^ Coordenadas a alterar
                  -> Piece -- ^ Nova peça
                  -> Maze -- ^ Labirinto 
                  -> Maze -- ^ Labirinto atualizado
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

-- | Função que substitui um elemento numa lista
replaceNElem :: Int -- ^ Índice em que o elemento deve ser inserido
             -> a -- ^ Elemento a inserir
             -> [a] -- ^ Lista
             -> [a] -- ^ Lista atualizada
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- | Função que devolve as coordenadas de um jogador
getPlayerCoords :: Player -- ^ Jogador
                -> Coords -- ^ Coordenadas
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Função que devolve o nº de vidas de um jogador
getPlayerLives :: Player -- ^ Jogador
               -> Int -- ^ Nº de vidas
getPlayerLives (Pacman (PacState (x,y,z,t,h,l) b c d )) = l
getPlayerLives (Ghost (GhoState (x,y,z,t,h,l) b )) = l

-- | Função que altera o nº de vidas de um jogador
setPlayerLives :: Int -- ^ Nº de vidas
               -> Player -- ^ Jogador
               -> Player -- ^ Jogador com o nº de vidas atualizado
setPlayerLives l' (Pacman (PacState (x,y,z,t,h,l) b c d )) = (Pacman (PacState (x,y,z,t,h,l') b c d ))
setPlayerLives l' (Ghost (GhoState (x,y,z,t,h,l) b )) = (Ghost (GhoState (x,y,z,t,h,l') b ))

-- | Função que elimina todas as ocorrências de um elemento de uma lista
delete :: Eq a => a  -- ^ Elemento a remover
               -> [a] -- ^ Lista
               -> [a] -- ^ Lista atualizada
delete deleted xs = [ x | x <- xs, x /= deleted ]

-- | Função que altera o estado de um fantasma
setGhostMode :: GhostMode -- ^ Estado do fantasma
             -> Player -- ^ Fantasma
             -> Player -- ^ Fantasma com o estado atualizado
setGhostMode m (Ghost (GhoState (x,y,z,t,h,l) b)) = (Ghost (GhoState (x,y,z,t,h,l) m))
setGhostMode _ x = x

-- | Função que altera o estado de um jogador
setPlayerState :: PlayerState -- ^ Estado
               -> Player -- ^ Jogador
               -> Player -- ^ Jogador com o estado atualizado
setPlayerState x' (Pacman (PacState x t o p)) = (Pacman (PacState x' t o p))
setPlayerState x' (Ghost (GhoState x b)) = (Ghost (GhoState x' b))

-- | Função que devolve a velocidade de um jogador
getPlayerVelocity :: Player -- ^ Jogador
                  -> Double -- ^ Velocidade
getPlayerVelocity (Pacman (PacState (x,y,z,t,h,l) b c d )) = z
getPlayerVelocity (Ghost (GhoState (x,y,z,t,h,l) b )) = z

-- | Função que altera a valocidade de um jogador
setPlayerVelocity :: Double -- ^ Velocidade
                  -> Player -- ^ Jogador 
                  -> Player -- ^ Jogador com a velocidade atualizada
setPlayerVelocity z' (Pacman (PacState (x,y,z,t,h,l) b c d )) = (Pacman (PacState (x,y,z',t,h,l) b c d ))
setPlayerVelocity z' (Ghost (GhoState (x,y,z,t,h,l) b )) = (Ghost (GhoState (x,y,z',t,h,l) b ))

-- | Função que altera o estado da boca de um Pacman 
setPacmanMouth :: Mouth -- ^ Estado da boca
               -> Player -- ^ Pacman 
               -> Player -- ^ Pacman com o estado da boca atualizado
setPacmanMouth c' (Pacman (PacState a b c d )) = (Pacman (PacState a b c' d))

-- | Função que devolve o estado da boca de um Pacman
getPacmanMouth :: Player -- ^ Pacman
               -> Mouth -- ^ Estado da boca do Pacman 
getPacmanMouth (Pacman (PacState a b c d)) = c

-- | Função que devolve o tempo Mega de um pacman
getMegaTime :: Player -- ^ Pacman
            -> Double -- ^ Tempo Mega
getMegaTime (Pacman (PacState a b c d)) = b

-- | Função que altera o tempo Mega de um pacman
setMegaTime :: Double -- ^ Tempo Mega
            -> Player -- ^ Pacman
            -> Player -- ^ Pacman com o tempo mega atualizado
setMegaTime b' (Pacman (PacState a b c d)) = (Pacman (PacState a b' c d))

-- | Função que devolve o tipo de um jogador
getPlayerType :: Player -- ^ Jogador
              -> String -- ^ Representação textual do tipo de jogador
getPlayerType (Pacman _) = "Pacman"
getPlayerType (Ghost _) = "Ghost"

-- | Função que devolve uma lista de jogadores a partir de um estado
getPlayerList :: State -- ^ Estado atual de jogo
              -> [Player] -- ^ Lista de jogadores associado ao estado atual
getPlayerList (State m ps lvl) = ps




{- |

= Introdução

Esta tarefa consistiu na criação de um bot, isto é, de um "jogador" capaz de jogar sem qualquer input.
Esta tarefa foi aquela em que sentimos uma maior dificuldade na medida em que nos  obrigou a pensar em
muitas variáveis e possibilidades para o comportamento que o bot deve assumir.

= Objetivos

O principal objetivo foi criar um bot capaz de simular o comportamento de um jogador humano. Para cumprir
esse objetivo, definimos várias funções para o bot saber o que fazer.

Idealmente, o bot deveria:

* Desviar-se do fantasma mais próximo de si;
* Desviar-se dos restantes fantasmas;
* Movimentar-se no sentido de atingir peças do tipo Food Big ou Food Little.

Uma vez que não fomoms capazes de implementar todas as funcionalidades pretendidas, o bot determina qual o
fantasma que se encontra mais próximo dele e move-se numa orientação de modo a evitá-lo. Como trabalho futuro
poderíamos melhorar o algoritmo do bot na medida em que, ao evitar apenas o fantasma mais próximo, isto conduz
a ótimos locais, isto é, decisões que ao nível individual são acertadas, mas ao nível global são erradas.
Desta forma, o bot além de se desviar deste fantasma, deveria também tentar evitar os outros fantasmas e,
se possível, movimentar-se no sentido de atingir peças do tipo Food Big ou Food Little.

= Conclusão

Em conclusão, apesar de não termos conseguido fazer tudo o que pretendíamos, fomos capazes de implementar
um bot funcional capaz de jogar sem qualquer input humano.

-}



-- Module: Tarefa6
-- Description: Implementação de um robot que joga automaticamente.

-- | Função que determina
bot :: Int -- ^ Identificador do jogador.
    -> State -- ^ Estado atual do jogo.
    -> Maybe Play -- ^ Jogada do pacman. O tipo Maybe modela a possibilidade de o robot não efetuar nenhuma jogada.
bot id e@(State m ps l) = if countFood m == 0
                          then Nothing
                          else return jogada
    where player = ps !! index
          index = fromJust . elemIndex id $ map getPlayerID ps
          jogada = Move id (getToPosition e id target)
          botCoords = getPlayerCoords . fst . head . filter ((== id) . snd) $ map (\p -> (p, getPlayerID p)) ps
          allCoords = [(x, y) | x <- [0 .. comprimento - 1], y <- [0 .. nCorredores - 1]] -- todas as coordenadas do mapa
          nCorredores = length m
          comprimento = length $ head m
          ghostCoords = map getPlayerCoords $ filter isGhost ps
          fantasmaMaisPertoCoords = minimumBy (compare `on` (dist botCoords)) ghostCoords
          target = maximumBy (compare `on` (dist fantasmaMaisPertoCoords)) (allCoords \\ ghostCoords)

-- | Função que conta o número de peças Food Big ou Food Little
countFood :: Maze -- ^ Labirinto
          -> Int -- ^ Número de peças Food Big ou Food Little
countFood = length . filter isFood
    where isFood $ Food Big = True 
          isFood $ Food Little = True
          isFood _ = False
{- |

= Introdução

Esta tarefa consistiu na implementação do comportamento dos fantasmas, sendo que existem dois tipos de jogada
distintas, uma quando o fantasma está no modo Alive, sendo que neste caso pretende-se que este persiga o Pacman,
e outra quando está no modo Dead, caso em que se pretende que que o fantasma fuja do Pacman.

= Objetivos

Quando o fantasma está no modo Alive, pretende-se que este persiga o Pacman. Desta forma, este deve efetuar jogadas
do tipo chaseMode e, para isso, determina as coordenadas do mapa em que o Pacman se encontra e, em função disso,
qual a orientação da próxima jogada.

Por outro lado, quando está no modo Dead, pretende-se que implemente uma forma de fuga do Pacman. Desta forma, 
deve efetuar jogadas do tipo scatterMode. Uma vez que não fomos capazes de implementar o algoritmo de fuga descrito
no enunciado, o fantasma começa por determina as coordenadas do mapa em que o Pacman se encontra e, posteriormente
determina qual as coordenadas no mapa que estão mais distantes do Pacman, movimentando-se, por fim nessa direção.

= Conclusão

Em conclusão, apesar de o fantasma não implemente a forma de fuga descrita no enunciado, i.e. este não se move
para a sua direita, circulando a área onde se encontra no sentido dos ponteiros do relógio, este é capaz de 
fugir do Pacman.

-}



-- Module: Tarefa5
-- Description: Implementação da movimentação de fantasmas.

-- | Função que devolve um conjunto de jogadas, uma de cada fantasma, com a melhor alternativa que cada consegue para reagir ao Pacman.
ghostPlay :: State -- ^ Estado atual.
          -> [Play] -- ^ Conjunto de jogadas, uma de cada fantasma, que os fantasmas devem efetuar.
ghostPlay e@(State m ps l) = map (getPlay e) $ mapGhostId ps 

-- | Função que determina a jogada que um fantasma deve tomar quando está no modo Alive.
chaseMode :: State -- ^ Estado atual.
          -> Int -- ^ Identificador do fantasma.
          -> Play -- ^ Jogada que o fantasma deve efetuar.
chaseMode e@(State m ps l) id = Move id (orientacaoJogada e id)

-- | Função que determina a orientação de uma jogada do fantasma no modo chaseMode
orientacaoJogada :: State -- ^ Estado atual.
                 -> Int -- ^ Identificador do fantasma.
                 -> Orientation -- ^ Orientação da jogada
orientacaoJogada e@(State m ps l) id
    | px > gx && (not $ isWall (gx+1, gy) m) = D
    | px <= gx && (not $ isWall (gx-1, gy) m) = U 
    | py > gy && (not $ isWall (gx, gy+1) m) = R
    | py <= gy && (not $ isWall (gx, gy-1) m) = L
    | otherwise  = Null 
    where (px, py) = getPlayerCoords . head $ filter (not . isGhost) ps
          (gx, gy) = getPlayerCoords . head $ filter (\p -> getPlayerID p == id) ps

-- | Função que determina a jogada que um fantasma deve tomar quando está no modo Dead.
scatterMode :: State -- ^ Estado atual.
            -> Int -- ^ Identificador do fantasma.
            -> Play -- ^ Jogada que o fantasma deve efetuar.
scatterMode e@(State m ps l) id = Move id orientation 
    where orientation = getToPosition e id coord
          coord = maximumBy (compare `on` (dist pacmanCoords)) (allCoords \\ ghostCoords) -- posicao mais distante do pacman
          pacmanCoords = getPlayerCoords . head $ filter (not . isGhost) ps -- posicao do pacman
          ghostCoords = map getPlayerCoords $ filter isGhost ps
          allCoords = [(x, y) | x <- [0 .. comprimento - 1], y <- [0 .. nCorredores - 1]] -- todas as coordenadas do mapa
          nCorredores = length m
          comprimento = length $ head m

-- | Função que determina a orientação de uma jogada do fantasma no modo scatterMode
getToPosition :: State -- ^ Estado atual.
              -> Int -- ^ Identificador do fantasma.
              -> Coords -- ^ Coordenadas onde se pretende que o fantasma chegue.
              -> Orientation -- ^ Orientação da jogada
getToPosition e@(State m ps l) id (x, y)
    | gx > x = L
    | gx <= x = R
    | gy > y = D
    | gx <= y = U
    | otherwise = Null
        where (gx, gy) = getPlayerCoords . head $ filter (\p -> getPlayerID p == id) ps

-- | Função que calcula a distância entre dois pontos
dist :: Coords -- ^ Coordenadas do ponto 1 
     -> Coords -- ^ Coordenadas do ponto 1 
     -> Double -- ^ Distância entre os jogadores
dist (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (x2 - x1)^2 + (y2 - y1)^2

-- | Função que determina uma jogada para um fantasma
getPlay :: State -- ^ Estado atual do jogo
        -> (Int, Player) -- ^ Fantasma e respetivo ID
        -> Play
getPlay e@(State m ps l) (id, p@(Ghost gstate)) = if ghostMode gstate == Alive
                                                  then chaseMode e id
                                                  else scatterMode e id

-- | Função que dada a lista de jogadores devolve uma lista com os fantasmas e respetivos IDs
mapGhostId :: [Player] -- ^ Lista de jogadores
           -> [(Int, Player)] -- ^ Lista de fantasmas e respetivo ID
mapGhostId ps = map (\p -> (getPlayerID p, p)) $ filter isGhost ps

-- | Função que determina de um determinado jogador é um fantasma.
isGhost :: Player -- ^ Jogador
        -> Bool -- ^ True caso o jogador seja um fantasma, false caso contrário
isGhost (Ghost _) = True
isGhost  _ = False

-- | Função que determina se uma peça é uma parede
isWall :: Coords -- ^ Coordenadas
       -> Maze -- ^ Labirinto
       -> Bool -- ^ True caso a peça seja uma parede, false caso contrário
isWall (x, y) m = (getPiece (x, y) m) == Wall 
        where getPiece (x, y) m = (m !! x) !! y 
{- |

= Introdução

Esta tarefa consiste na implementação de uma função que altera o estado do jogo perante a passagem do tempo, a função passTime,
função esta que será a que irá executar todas as jogadas de todos os jogadores.

= Objetivos

Nesta tarefa focamo-nos apenas nas jogadas quando a velocidade do jogador é 1 ou 0.5, isto porque não foi possível implementar 
mais velocidades até à entrega do projeto. Sendo assim,

 * Quando a velocidade do jogador é 1, este jogador efetua uma jogada a cada instância.
 * Quando a velocidade do jogador é 0.5, este jogador efetua uma jogada a cada 2 instâncias.

Com esta permissa, a função irá efetuar jogadas consoante a instância do jogo seja um número par ou ímpar.
Caso seja número par, todos os jogadores irão efetuar as suas jogadas.
Caso seja número ímpar, apenas os jogadores com velocidade 1 irão efetuar as suas jogadas.

Uma pequena alteração que foi feita é que a função passTime apenas atualiza as jogadas dos fantasmas que não são controlados 
mas as jogadas dos jogadores controlados pelo utilizador são atualizadas ao mesmo tempo que a função passTime é executada, 
na função nextFrame implementada no ficheiro Main.hs, sendo o efeito da função passTime complementado e assim completo.

= Conclusão

Em suma, apesar da função passTime não possuir todos as características pedidas em si, 
o seu trabalho acaba por ser igualmente feito e permite ao jogo progredir com a passagem do tempo.

-}


-- Module: Tarefa4
-- Description: Implementação do efeito da passagem de um instante de tempo num determinado estado do jogo

-- | Intervalo de tempo pré-determinado entre a atualização anterior e a próxima atualização.
defaultDelayTime :: Integer -- ^ Número
defaultDelayTime = 250 -- 250 ms

-- | Função que efetua as jogadas de todos os jogadores
passTime :: Int -- ^ Step : Número que indica por quantas iterações o jogo já passou.
         -> State -- ^ Estado atual.
         -> State -- ^ Novo estado atualizado.
passTime t e@(State m ps l) | even t = ghostsPlay (ghostPlay e) e
                            | odd t = ghostNormalMovement (ghostPlay e) e

-- | Função que efetua as jogadas de todos os fantasmas
ghostsPlay :: [Play] -- ^ Lista de jogadas efetuadas pelos fantasmas
           -> State -- ^ Estado atual de jogo
           -> State -- ^ Novo estado atualizado
ghostsPlay [] s = s
ghostsPlay s ps = ghostNormalMovement s (ghostDeadMovement s ps)

-- | Função que efetua as jogadas dos fantasmas vivos (com velocidade 1)
ghostNormalMovement :: [Play] -- ^ Lista de jogadas efetuadas pelos fantasmas
                    -> State -- ^ Estado atual de jogo
                    -> State -- ^ Novo estado atualizado
ghostNormalMovement [] s = s
ghostNormalMovement (x@(Move id o):t) s@(State m ps l) | getPlayerVelocity ghost == 1 = ghostNormalMovement t (play x s)
                                                       | otherwise = ghostNormalMovement t s
                                               where ghost = ps !! (fromJust . elemIndex id $ map getPlayerID ps)
-- | Função que efetua as jogadas dos fantasmas mortos (com velocidade 0.5)
ghostDeadMovement :: [Play] -- ^ Lista de jogadas efetuadas pelos fantasmas
                  -> State -- ^ Estado atual de jogo
                  -> State -- ^ Novo estado atualizado
ghostDeadMovement [] s = s
ghostDeadMovement (x@(Move id o):t) s@(State m ps l) | getPlayerVelocity ghost == 0.5 = ghostDeadMovement t (play x s)
                                                     | otherwise = ghostDeadMovement t s 
                                               where ghost = ps !! (fromJust . elemIndex id $ map getPlayerID ps)











-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.

-- | Module: Tarefa 3
-- | Description: Através de um labirinto do tipo Maze, tranformá-lo num conjunto de instruções mais compactas do tipo Instructions

-- | Função que calcula o nº de instruções de uma lista de instruções.
sizeInstructions :: Instructions -- ^ Conjunto de instruções
                 -> Int -- ^ Número de instruções
sizeInstructions [] = 0
sizeInstructions l = sum $ map sizeup l
          where sizeup (Instruct []) = 0
                sizeup (Instruct (x:xs)) = sizeup (Instruct xs) + 1
                sizeup (Repeat _) = 1

-- | Função principal que compacta e transforma um labirinto num conjunto de instruções.
compactMaze :: Maze -- ^ Labirinto
            -> Instructions -- ^ Conjunto de instruções comprimidas
compactMaze [] = []
compactMaze l = compactInstrucVert (compactInstrucs . mazeToInstruct $ l) 0


-- | Função que comprime um conjunto de instruções substituindo os conjuntos repetidos.
compactInstrucVert :: Instructions -- ^ Conjunto de instruções 
                   -> Int -- ^ Acumulador que armazena a posição da instrução
                   -> Instructions -- ^ Conjunto de instruções 
compactInstrucVert [] _ = []
compactInstrucVert (x@(Repeat y):s) ac = x : compactInstrucVert s (ac+1)
compactInstrucVert (x:s) ac | elem x s = compactInstrucVert l ac
                            | otherwise = x:compactInstrucVert s (ac+1)
                               where l = x : repeatInstruc x s ac

-- | Função que sustitui conjunto de instruções iguais por "Repeat i" sendo i a posição da 1ª occorrência do conjunto de instruções iguais.
repeatInstruc :: Instruction  -- ^ Instrução
              -> Instructions -- ^ Conjunto de instruções
              -> Int  -- ^ Acumulador que armazena a posição da instrução
              -> Instructions -- ^ Conjunto de instruções
repeatInstruc _ [] _ = []
repeatInstruc x (y:s) ac | x==y = Repeat ac : repeatInstruc x s ac
                         | otherwise = y: repeatInstruc x s ac 

-- | Função que junta as instruções comprimidas num conjunto de instruções 
compactInstrucs :: Instructions -- ^ Conjunto de instruções 
                -> Instructions -- ^ Conjunto de instruções semi-compactadas
compactInstrucs [] = []
compactInstrucs (x:s) = Instruct (compactInstrucHoriz x) : compactInstrucs s


-- | Função que comprime uma lista de instruções agrupado múltiplas instruções consecutivas numa só.
compactInstrucHoriz :: Instruction  -- ^ Lista de instruções
                    -> [(Int,Piece)] -- ^ Lista de tuplos formados por valores inteiros e peças
compactInstrucHoriz (Instruct [a]) = [a]
compactInstrucHoriz (Instruct ((x,y):(a,b):t)) | y == b = compactInstrucHoriz (Instruct (xd:t))
                                               | otherwise = ((x,y):compactInstrucHoriz (Instruct ((a,b):t)))
                                                   where xd = (x+a,y)

-- | Função que junta instruções que formam um corredor num conjunto de instruções para formar o labirinto.
mazeToInstruct :: Maze  -- ^ Labirinto
               -> Instructions -- ^ Conjunto de instruções
mazeToInstruct [] = []
mazeToInstruct (x:s) = Instruct (corridorToInstruct x) : mazeToInstruct s


-- | Função que transforma um corredor numa lista de instruções.
corridorToInstruct :: Corridor  -- ^ Corredor
                   -> [(Int,Piece)] -- ^ Lista de tuplos formados por valores inteiros e peças
corridorToInstruct [] = []
corridorToInstruct (x:s) | x == Wall = ((1,Wall):corridorToInstruct s)
                         | x == Empty = ((1,Empty):corridorToInstruct s)
                         | x == Food Little = ((1,Food Little):corridorToInstruct s)
                         | x == Food Big = ((1,Food Big):corridorToInstruct s)



-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.


-- Module : Tarefa2
-- Description : Implementação de realização de jogadas para jogadores do tipo Pacman

play :: Play -- ^ Jogada
     -> State -- ^ Estado atual do jogo 
     -> State -- ^ Novo estado após a jogada ser efetuada
play p@(Move i o) e@(State m ps l) =  State m' (replaceNElem index newplayer newghosts) l 
    where player = ps !! index
          index = fromJust . elemIndex i $ map getPlayerID ps
          newplayer = if getPlayerType player == "Ghost"
                      then setPlayerCoords player (updatedCoords p e)
                      else startMega p e (updatePacmanMode $ updatePacmanMouth $ setPacmanMode (updatedState p e) $ setPlayerState newpacstate player)
          newghosts = ghosts p e
          newpacstate = (id, coords, v, o, points, li) -- (ID,  (x,y), velocity, orientation, points, lives)
          id = getPlayerID player
          coords = updatedCoords p e
          v = getPlayerVelocity player
          points = updatedScore p e
          li = updatedLives p e
          casaCoords = (length m `div` 2, (length . head $ m) `div` 2)
          m' = if getPiece (getPlayerCoords player) m `elem` [Food Big, Food Little]
                  then replaceElemInMaze (getPlayerCoords player) Empty m
                  else m

-- | Função que determina as coordenadas dos fantasmas mortos
getDeadGhostCoords :: [Player] -- ^ Lista de Jogadores
                   -> [Coords] -- ^ Coordenadas dos fantasmas mortos
getDeadGhostCoords = map getPlayerCoords . filter isDeadGhost
    where isDeadGhost (Ghost (GhoState _ Dead)) = True
          isDeadGhost _ = False 

-- | Função que determina as coordenadas dos fantasmas vivos
getAliveGhostCoords :: [Player] -- ^ Lista de Jogadores
                    -> [Coords] -- ^ Coordenadas dos fantasmas mortos
getAliveGhostCoords = map getPlayerCoords . filter isAliveGhost
    where isAliveGhost (Ghost (GhoState _ Alive)) = True
          isAliveGhost _ = False 

-- | Função que devolve o fantasma que se encontra numas determinadas coordenadas
getGhost :: Coords -- ^ Coordenadas
         -> [Player] -- ^ Lista de jogadores
         -> Player -- ^ Fantasma
getGhost c js = head $ filter (\j -> isGhost j && getPlayerCoords j == c) js
    where isGhost (Ghost _) = True
          isGhost  _ = False

-- | Função que devolve uma peça dadas as suas coordenadas
getPiece :: (Int, Int) -- ^ Coordenadas
         -> Maze -- ^ Labirinto
         -> Piece -- ^ Peça
getPiece (x, y) m = (m !! x) !! y 

-- | Função que atualiza as coordenadas do jogador após a jogada
updatedCoords :: Play -- ^ Jogada
              -> State -- ^ Estado do jogo
              -> Coords -- ^ Coordenadas do jogador após a jogada
updatedCoords p@(Move i o) e@(State m ps l)
    | getPlayerOrientation player /= o || o == Null = (x, y)
    | o == L && y == 0 = (x, comprimento - 1)
    | o == R && y == comprimento - 1 = (x, 0)
    | o == L = if y == 0 || getPiece (x, y-1) m == Wall then (x, y) else (x, y-1)
    | o == R = if y == comprimento - 1 || getPiece (x, y+1) m == Wall then (x, y) else (x, y+1)
    | o == U = if x == 0 || getPiece (x-1, y) m == Wall then (x, y) else (x-1, y)
    | o == D = if x == nCorredores - 1 || getPiece (x+1, y) m == Wall then (x, y) else (x+1, y)
    | otherwise = (x, y)
        where   player = ps !! (fromJust . elemIndex i $ map getPlayerID ps)
                (x, y) = getPlayerCoords player
                comprimento = length . head $ m
                nCorredores = length m
             
-- | Função que calcula a pontuação de um jogador após uma jogada   
updatedScore :: Play -- ^ Jogada
             -> State -- ^ Estado do jogo 
             -> Int -- ^ Pontuação no final da jogada
updatedScore p@(Move i o) e@(State m ps l)
    | o /= getPlayerOrientation player = getPlayerPoints player
    | (updatedCoords p e) `elem` getDeadGhostCoords ps = 10 + getPlayerPoints player
    | piece == Food Little = 1 + getPlayerPoints player
    | piece == Food Big = 5 + getPlayerPoints player
    | otherwise = getPlayerPoints player
    where player = ps !! (fromJust . elemIndex i $ map getPlayerID ps)
          piece = getPiece (updatedCoords p e) m
                
-- | Função que calcula o número de vidas de um jogador após uma jogada   
updatedLives :: Play -- ^ Jogada
             -> State -- ^ Estado do jogo  
             -> Int -- ^ Número de vidas após a jogada
updatedLives p@(Move i o) e@(State m ps l) = if (updatedCoords p e) `elem` getAliveGhostCoords ps
                                             then pred $ getPlayerLives player
                                             else getPlayerLives player
                                             where player = ps !! (fromJust . elemIndex i $ map getPlayerID ps)

-- | Função que determina o novo modo de um jogador
updatedState :: Play -- ^ Jogada
             -> State -- ^ Estado do jogo
             -> PacMode -- ^ Novo modo do Pacman
updatedState p@(Move i o) e@(State m ps l) = if getPlayerLives player == 1 && (updatedCoords p e) `elem` getAliveGhostCoords ps
                                             then Dying
                                             else if (updatedCoords p e) `elem` getDeadGhostCoords ps
                                                  then Mega
                                                  else getPacmanMode player
            where player = ps !! (fromJust . elemIndex i $ map getPlayerID ps)


-- | Função que atualiza os fantasmas de uma lista de Players quando um PacMan come uma comida grande ou o fantasma é comido pelo PacMan
ghosts :: Play -- ^ Jogada
       -> State -- ^ Estado do jogo
       -> [Player] -- ^ Lista de fantasmas atualizada
ghosts p@(Move i o) e@(State m ps l) | piece == Food Big = ghostDead ps
                                     | (updatedCoords p e) `elem` getDeadGhostCoords ps = afterDeathGhost ps (updatedCoords p e)
                                     | otherwise = map (setGhostMode Alive) ps
                                           where piece = getPiece (updatedCoords p e) m

-- | Função que muda o modo de todos os fantasmas para Dead e a velocidade para metade
ghostDead :: [Player] -- ^ Lista de jogadores em jogo
          -> [Player] -- ^ Lista de jogadores atualizada
ghostDead [] = []
ghostDead (x:xs) | isGhost x = (setPlayerVelocity newvelocity (setGhostMode Dead x)):(ghostDead xs)
                 | otherwise = x:(ghostDead xs)
                                    where newvelocity = (getPlayerVelocity x) / 2
                                          isGhost (Ghost _) = True
                                          isGhost  _ = False

-- | Função que altera a velocidade e o modo de um fantasma comido para o dobro (velocidade normal) e Alive, respetivamente
afterDeathGhost :: [Player] -- ^ Lista de jogadores em jogo
                -> Coords -- ^ Coordenadas do fantasma morto
                -> [Player] -- ^ Lista de jogadores atualizada
afterDeathGhost [] _ = []
afterDeathGhost (x:xs) c | isGhost x && c == getPlayerCoords x = (setPlayerVelocity newvelocity (setGhostMode Alive (setPlayerCoords x c))) : (afterDeathGhost xs c)
                         | otherwise = x : afterDeathGhost xs c
                               where newvelocity = (getPlayerVelocity x) * 2
                                     isGhost (Ghost _) = True
                                     isGhost  _ = False

-- | Função que atualiza a boca do Pacman
updatePacmanMouth :: Player -- ^ Pacman
                  -> Player -- ^ Pacman com a boca atualizada
updatePacmanMouth s | getPacmanMouth s == Closed = setPacmanMouth Open s
                    | otherwise = setPacmanMouth Closed s

-- | Função que atualiza o modo do Pacman
updatePacmanMode :: Player -- ^ Pacman
                 -> Player -- ^ Pacman com o modo atualizado
updatePacmanMode s | getPacmanMode s == Mega && getMegaTime s <= 0 = setPacmanMode Normal (setMegaTime 0 s)
                   | getMegaTime s > 0 = setPacmanMode Mega (setMegaTime ((getMegaTime s)-1) s)
                   | otherwise = s

-- | Função que insere tempo Mega a jogador Pacman
startMega :: Play -- ^ Jogada
          -> State -- ^ Estado atual do jogo
          -> Player -- ^ Jogador
          -> Player -- ^ Jogador atualizado
startMega p@(Move i o) e@(State m ps l) player | piece == Food Big = setMegaTime 40 player
                                               | otherwise = player
           where piece = getPiece (updatedCoords p e) m


-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.


-- Module : Tarefa1
-- Description : Implementação de um mecanismo de geração de labirintos.

-- | Função que gera um Labirinto dado o comprimento de cada corredor, o número de corredores e uma semente de aleatoriedade.
generateMaze :: Int -- ^ Comprimento de cada Corredor.
             -> Int -- ^ Número de Corredores a gerar.
             -> Int -- ^ Semente de aleatoriedade.
             -> Maze -- ^ O Labirinto que a função gera a partir dos parâmetros fornecidos.
generateMaze comprimento ncorredores seed = tunnel
                                            . generateGhostHouse
                                            $ walls
                                            ++ (map (([Wall] ++) . (++ [Wall]) . generateCorridor)
                                            . chunksOf (comprimento - 2)
                                            $ generateRandoms ((comprimento - 2) * (ncorredores - 2)) seed)
                                            ++ walls
                                            where walls = [replicate comprimento Wall]

-- | Gerador de números aleatórios
generateRandoms :: Int -- ^ Quantidade de nºs gerados
                -> Int -- ^ semente aleatória
                -> [Int] -- ^ Lista de nºs aleatórios
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen

-- | Função que gera uma peça em função de um inteiro aleatório.
generatePiece :: Int -- ^ Inteiro utilizado para gerar a peça.
              -> Piece -- ^ Peça gerada a partir dos parâmetros fornecidos.
generatePiece n 
        | n == 3 = Food Big
        | n >= 0 && n < 70 = Food Little
        | n >= 70 && n <= 99 = Wall

-- | Função que gera um Corredor a partir de uma lista de inteiros.
generateCorridor :: [Int] -- ^ Lista de inteiros utilizada para gerar o Corredor.
                 -> Corridor -- ^ Corredor gerado a partir dos parâmetros fornecidos.
generateCorridor = map generatePiece

-- | Função que adiciona a casa dos fantasmas a um Labirinto.
generateGhostHouse :: Maze -- ^ Labirinto inicial
                   -> Maze -- ^ Labirinto resultante
generateGhostHouse l | even (length l) = (take (a-3) l) ++ casa b 0 ++ (drop (a+2) l) -- consoante o nº de corredores ser par ou ímpar, é inserida a casa de fantasmas adequada
                     | odd (length l) = (take (a-2) l) ++ casa c 0 ++ (drop (a+3) l)
                                        where a = (div (length l) 2)
                                              b = drop (a-3) l
                                              c = drop (a-2) l


-- | Função que irá inserir a casa de fantasmas nos corredores centrais do labirinto
casa :: Maze -- ^ Labirinto inicial
     -> Int -- ^ Acumulador de controlo
     -> Maze -- ^ Labirinto resultante
casa _ 5 = []
casa (x:xs) y = casa1 x y: casa xs (y+1) -- insere a casa de fantasmas no meio dos corredores centrais

-- | Função auxiliar da função casa
casa1 :: Corridor -- ^ Corredor inicial
      -> Int -- ^ Acumulador de controlo
      -> Corridor -- ^ Corredor final
casa1 l y | even (length l) = (take (div (length l) 2 -5) l) ++ (casap !! y) ++ (drop (div (length l) 2 +5) l) -- parte cada corredor em duas partes, remove os elementos centrais 
          | odd (length l) = (take (div (length l) 2 -5) l) ++ (casai !! y) ++ (drop (div (length l) 2 +6) l) -- e substitui-os com corredores que formarão a casa dos fantasmas.

-- casa de fantasmas para nº de corredores par
casap = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
-- casa de fantasmas para nº de corredores ímpar
casai = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],[Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],[Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

-- | Função para inserir o túnel
tunnel :: Maze -- ^ Labirinto sem túnel
       -> Maze -- ^ Labirinto com túnel
tunnel x | even (length x) = take (a-1) x ++ [[Empty] ++ drop 1 (init(x !! (a-1))) ++ [Empty]] ++ [[Empty] ++ drop 1 (init(x !! a)) ++ [Empty]] ++ drop (a+1) x
         | odd (length x) = take a x ++ [[Empty] ++ drop 1 (init(x !! a)) ++ [Empty]] ++ drop (a+1) x
                   where a = (div (length x) 2)





-- | Função que carrega um mapa em formato .txt
loadManager :: Manager -- ^ Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime)

-- | Função que dada uma tecla do teclado altera a orientação do jogador controlado
updateControlledPlayer :: Key -- ^ Tecla do teclado
                       -> Manager -- ^ Manager atual
                       -> Manager -- ^ Manager atualizado
updateControlledPlayer k (Manager s p step bef delt del) = case k of KeyUpArrow -> (Manager (changePlayerOrientation p U s) p step bef delt del) 
                                                                     KeyLeftArrow ->  (Manager (changePlayerOrientation p L s) p step bef delt del) 
                                                                     KeyRightArrow ->  (Manager (changePlayerOrientation p R s) p step bef delt del) 
                                                                     KeyDownArrow ->  (Manager (changePlayerOrientation p D s) p step bef delt del) 

-- | Função que altera a orientação de um jogador num certo estado de jogo.
changePlayerOrientation :: Int -- ^ Identificador do jogador
                        -> Orientation -- ^ Orientação pretendida
                        -> State -- ^ Estado atual
                        -> State -- ^ Estado atualizado
changePlayerOrientation id o s@(State m ps lvl) = State m (replaceNElem index (setPlayerOrientation o player) ps) lvl
             where player = ps !! index
                   index = fromJust . elemIndex id $ map getPlayerID ps

-- | Função que atualiza o ecrã
updateScreen :: Window -- ^ Janela
             -> ColorID -- ^ Identificador de cor
             -> Manager -- ^ Manager
             -> Curses () -- ^ Output do NCurses
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render

-- | Função que devolve o tempo em milisegundos
currentTime :: IO Integer -- ^ Valor de tempo em milisegundos
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

-- | Função que atualiza o tempo de um Manager
updateTime :: Integer -- ^ Valor de tempo atual
           -> Manager -- ^ Manager atual
           -> Manager -- ^ Manager atualizado
updateTime now (Manager s p st bef delta delay) = Manager s p st bef (now - bef) delay 

-- | Função que reinicia o contador
resetTimer :: Integer -- ^ Valor de tempo atual
           -> Manager -- ^ Manager atual
           -> Manager -- ^ Manager atualizado
resetTimer now (Manager s p st bef delta delay) = Manager s p st now 0 delay

-- | Função que gera o próximo frame
nextFrame :: Integer -- ^ Valor de tempo atual
          -> Manager -- ^ Manager atual
          -> Manager -- ^ Manager atualizado
nextFrame now (Manager state p step b d delay) = Manager (play (Move p (getPlayerOrientation player)) (passTime step state)) p (step+1) now 0 delay
                                    where player = ps !! (fromJust . elemIndex p $ map getPlayerID ps)
                                          ps = getPlayerList state

-- | Função que funciona como loop para o jogo funcionar
loop :: Window -- ^ Janela
     -> Manager -- ^ Manager atual
     -> Curses () -- ^ Output do NCurses
loop w man@(Manager s@(State m ps lvl) pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else if elem 0 (map (getPlayerLives) ps)
           then return ()
           else do
               ev <- getEvent w $ Just 0
               case ev of
                    Nothing -> loop w (updateTime now man)
                    Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                    Just ev' ->
                       if (ev' == EventCharacter 'q')
                         then return ()
                         else loop w (updateTime now man)

-- | Função principal que inicia o jogo
main :: IO () -- ^ Input/Output
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager




loadMaze :: String -> State
loadMaze filepath = unsafePerformIO $ readStateFromFile filepath


readStateFromFile :: String -> IO State
readStateFromFile f = do
                content <- readFile f
                let llines = lines content 
                    (new_map,pl) = convertLinesToPiece llines 0 []
                return (State new_map pl 1)



convertLinesToPiece :: [String] -> Int -> [Player] -> (Maze,[Player])
convertLinesToPiece [] _ l = ([],l)
convertLinesToPiece (x:xs) n l  = let (a,b) = convertLineToPiece x n 0 l 
                                      (a1,b1) = convertLinesToPiece xs (n+1) b
                                  in (a:a1,b1)

convertLineToPiece :: String -> Int -> Int -> [Player] -> ([Piece],[Player])
convertLineToPiece [] _ _ l = ([],l)
convertLineToPiece (z:zs) x y l = let (a,b ) = charToPiece z x y l
                                      (a1,b1) = convertLineToPiece zs x (y+1) b 
                                  in (a:a1,b1)


charToPiece :: Char -> Int -> Int -> [Player] -> (Piece,[Player])
charToPiece c x y l
    | c == '{' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Normal ))   :l) )
    | c == '<' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Closed Normal )) :l) )
    | c == '}' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Open Normal ))   :l) )
    | c == '>' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Closed Normal )) :l) )
    | c == 'V' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Open Normal ))   :l) )
    | c == 'v' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Closed Normal )) :l) )
    | c == '^' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Open Normal ))   :l) )
    | c == '|' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Closed Normal )) :l) )
    | c == 'X' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Dying ))    :l) )
    | c == 'M' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Alive))            :l) )
    | c == '?' =  (Empty, ((Ghost (  GhoState (length l,(x,y),0,Null,0,1 ) Dead))             :l) )
    | c == 'o' =  (Food Big,l)
    | c == '.' =  (Food Little,l)
    | c == '#' =  (Wall,l)
    | otherwise = (Empty,l)


data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

{-| ==Introdução 
Nesta Tarefa, foi-nos proposto realizar um bot que efetuava os movimentos do Jogador de forma autónoma. Para tal, decidimos utilizar um formato semelhante ao que utilizamos nos Fantasmas da Tarefa anterior.

==Objetivos
Para realizarmos um comportamento do bot, decidimos que este se iria mover em função da sua distância à Food Big mais próxima e, assim que se encontra um Fantasma nas suas próximidades, o Pac-Man vai-se movimentar no sentido contrário para evitar colisão.
Quando o Pac-Man se encontra no estado Mega, vai perseguir os Fantasmas da mesma forma que estes o perseguem,ou seja, vai procurar igualar o seu eixo y ao dos Fantasmas e depois vai ao encontro destes. 
Para conseguir detetar as Food Big e os Ghosts mais próximas, implementamos a função nearestCoord que devolve a Coordenada mais próxima em função da peça que estamos à procura.

==Discussão e Conclusão
Da mesma forma que na Tarefa 5 referi que o comportamento dos Fantasmas poderia ter ficado melhor no sentido em que é muito fácil de ser evitado, o movimento que implementamos para Pac-Man acaba por fazer dele uma "presa fácil", na medida em que a sua estratégia
de escapar, bem como a sua estratégia de perseguição não são ideiais. Apesar disso, creio que os comportamentos que implementamos vão de acordo ao nosso objetivo.
-}


-- | Função que devolve a lista das coordenadas das peças Food Big num dado Labirinto
foodLocation :: Maze -> Coords -> [Coords]
foodLocation [] (a,b)= []
foodLocation ([]:xs) (a,b) = foodLocation xs (a+1,0)
foodLocation ((y:ys):xs) (a,b) | y == Food Big = (a,b) : foodLocation (ys:xs) (a,b)
                               | otherwise = foodLocation (ys:xs) (a,b+1)

-- | Função que, dada uma lista de players, devolve aquela que se encontra mais próxima da coordenada introduzida
nearestCoord :: [Coords] -> Coords -> Coords
nearestCoord [(x1,y1),(x2,y2)] (a,b)  | distance (x1,y1) (a,b) < distance (x2,y2) (a,b) = (x1,y1)
                                      | otherwise = (x2,y2)
nearestCoord ((x1,y1):(x2,y2):(x3,y3):ys) (a,b)  | distance (x1,y1) (a,b) < distance (x2,y2) (a,b) = nearestCoord ((x1,y1):(x3,y3):ys) (a,b)
                                                 | otherwise = nearestCoord ((x2,y2):(x3,y3):ys) (a,b)
nearestCoord [] _ = (0,0)

-- |Função que define o comportamento do bot, que inclui as exceções todas para assegurar uma boa resposta pelo Pac-Man
botMovement :: State -> Coords -> Play
botMovement ((State m [Pacman (PacState (i,(x,y),ve,o,p,v) c mo Normal)] l)) (a,b) | isGhost (whichPiece (x,y+3) m) && whichPiece (x,y-1) m /= Wall = Move i L
                                                                                   | isGhost (whichPiece (x,y+3) m) && whichPiece (x,y-1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                                                      else Move i D
                                                                                   | isGhost (whichPiece (x,y+3) m) && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall = Move i D
                                                                                   | isGhost (whichPiece (x,y+3) m) && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i R
                                                                                   | isGhost (whichPiece (x,y-3) m) && whichPiece (x,y+1) m /= Wall = Move i R
                                                                                   | isGhost (whichPiece (x,y-3) m) && whichPiece (x,y+1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                                                      else Move i D
                                                                                   | isGhost (whichPiece (x,y-3) m) && whichPiece (x,y+1) m == Wall = Move i R
                                                                                   | isGhost (whichPiece (x,y-3) m) && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall = Move i D
                                                                                   | isGhost (whichPiece (x,y-3) m) && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i L
                                                                                   | isGhost (whichPiece (x+3,y) m) && whichPiece (x-1,y) m /= Wall = Move i L
                                                                                   | isGhost (whichPiece (x+3,y) m) && whichPiece (x-1,y) m == Wall = if distance (x,y+1)  (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                                      else Move i R
                                                                                   | isGhost (whichPiece (x+3,y) m) && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i D
                                                                                   | isGhost (whichPiece (x+3,y) m) && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i R
                                                                                   | isGhost (whichPiece (x-3,y) m) && whichPiece (x+1,y) m /= Wall = Move i L
                                                                                   | isGhost (whichPiece (x-3,y) m) && whichPiece (x+1,y) m == Wall = if distance (x,y+1)  (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                                      else Move i R
                                                                                   | isGhost (whichPiece (x-3,y) m) && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i D
                                                                                   | isGhost (whichPiece (x-3,y) m) && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i R
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = Move i D
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = Move i U
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Empty = Move i R
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Food Little = Move i L
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                                                                                                                                    else Move i D
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Empty = if distance (x+1,y) (a,b) > distance (x,y+1) (a,b) then Move i R
                                                                                                                                                                                                                                    else Move i D
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Food Little = if distance (x+1,y) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                                                                                                                    else Move i D
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = Move i U                                                                                                                                                 
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Food Little = if distance (x-1,y) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                                                                                                                    else Move i U
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Empty = if distance (x-1,y) (a,b) > distance (x,y+1) (a,b) then Move i R
                                                                                                                                                                                                                                    else Move i U
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Empty = Move i R
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Food Little = if distance (x,y+1) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                                                                                                                    else Move i R
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Food Little = Move i L
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                                                                                                                        else Move i D 
                                                                                   | whichPiece (x+1,y) m == Food Big && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = Move i D
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Food Big && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Empty = Move i U
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Food Big && whichPiece (x,y-1) m == Empty = Move i R
                                                                                   | whichPiece (x+1,y) m == Empty && whichPiece (x-1,y) m == Empty && whichPiece (x,y+1) m == Empty && whichPiece (x,y-1) m == Food Big = Move i L
                                                                                   | whichPiece (x+1,y) m == Food Big && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Food Little = Move i D
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Food Big && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Food Little = Move i U
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Food Big && whichPiece (x,y-1) m == Food Little = Move i R
                                                                                   | whichPiece (x+1,y) m == Food Little && whichPiece (x-1,y) m == Food Little && whichPiece (x,y+1) m == Food Little && whichPiece (x,y-1) m == Food Big = Move i L
                                                                                   | y >= (corridorSize m) - (div (corridorSize m) 4) && (mod (corridorSize m) 2 == 0 && b <= div (corridorSize m) 4) = if x > midHeight m && whichPiece (x-1,y) m /= Wall then Move i U
                                                                                                                                                                                               else if x < midHeight m && whichPiece (x+1,y) m /= Wall then Move i D
                                                                                                                                                                                               else if x == midHeight m && whichPiece (x,y+1) m /= Wall then Move i R
                                                                                                                                                                                               else Move i L      
                                                                                   | y >= ((corridorSize m) - (div (corridorSize m) 4)) && (mod (corridorSize m) 2 /= 0 && b <= div (corridorSize m) 4) = if x > midHeight m && whichPiece (x-1,y) m /= Wall then Move i U
                                                                                                                                                                                               else Move i L
                                                                                   | y <= ((corridorSize m) - (div (corridorSize m) 4)) && (mod (corridorSize m) 2 == 0 && b <= div (corridorSize m) 4) = if x > midHeight m && whichPiece (x+1,y) m /= Wall then Move i U
                                                                                                                                                                                               else if x < midHeight m && whichPiece (x+1,y) m /= Wall then Move i D
                                                                                                                                                                                               else if x == midHeight m && whichPiece (x,y+1) m /= Wall then Move i R
                                                                                                                                                                                               else Move i L
                                                                                   | y >= ((corridorSize m) - (div (corridorSize m) 4)) && (mod (corridorSize m) 2 /= 0 && b <= div (corridorSize m) 4) = if x > midHeight m && whichPiece (x-1,y) m /= Wall then Move i U
                                                                                                                                                                                                            else Move i D   
                                                                                   | otherwise = Move i L                                                                                                                  

botMovement (State m [Pacman (PacState (i,(x,y),ve,o,p,v) c mo Mega)] l) (a,b)     | y < b && whichPiece (x,y+1) m /= Wall = Move i R
                                                                                   | y < b && whichPiece (x,y+1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                           else Move i D
                                                                                   | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x+1,y) m == Wall = Move i U
                                                                                   | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x-1,y) m == Wall = Move i D
                                                                                   | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i L                                
                                                                                   | y < b && whichPiece (x,y-1) m /= Wall = Move i L
                                                                                   | y < b && whichPiece (x,y-1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                             else Move i D
                                                                                   | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall = Move i U
                                                                                   | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x-1,y) m == Wall = Move i D
                                                                                   | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i R
                                                                                   | y == b && x < a && whichPiece (x+1,y) m /= Wall = Move i D
                                                                                   | y == b && x < a && whichPiece (x+1,y) m == Wall = if distance (x,y+1) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                      else Move i R
                                                                                   | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y+1) m == Wall = Move i L
                                                                                   | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y-1) m == Wall = Move i R
                                                                                   | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y-1) m == Wall && whichPiece (x,y-1) m == Wall = Move i U
                                                                                   | y == b && x > a && whichPiece (x-1,y) m /= Wall = Move i U
                                                                                   | y == b && x > a && whichPiece (x-1,y) m == Wall = if distance (x,y+1) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                     else Move i R
                                                                                   | y == b && x > a && whichPiece (x-1,y) m == Wall && whichPiece (x,y+1) m == Wall = Move i L
                                                                                   | y == b && x > a && whichPiece (x-1,y) m == Wall && whichPiece (x,y+1) m == Wall && whichPiece (x,y-1) m == Wall = Move i D
                                                                                   | otherwise = Move i U

-- |Função que testa se um jogador é um Ghost ou não
playerDetectionv2:: Player->Bool
playerDetectionv2 (Ghost _) = True
playerDetectionv2 _ = False

-- | Função que, dado um state, devolve a lista de coordenadas de todo os Ghosts que se encontram neste state.
getGhostCoords:: State->[Coords]
getGhostCoords (State m (x:xs) l ) | playerDetectionv2 x = getPlayerCoords x : getGhostCoords (State m xs l)
                                   | otherwise = getGhostCoords (State m xs l)
getGhostCoords (State m [] l)= []


-- |Função final que aplica a função botMovement às coordenadas necessárias, nomeadamente as coordenadas dos fantasmas mais próximos e das Food Big mais próximas
bot :: Int->State-> Maybe Play
bot id (State m [] l) = Nothing
bot id (State m (x:xs) l) | playerDetection x && getPacmanMode x==Mega && getPlayerID x==id = Just (botMovement (State m [x] l) (a,b))
                          | playerDetection x && getPacmanMode x == Normal && getPlayerID x==id = Just (botMovement (State m [x] l) (a1,b1))
                          | otherwise=  bot id (State m xs l)
 where (a,b)= nearestCoord (getGhostCoords (State m (x:xs) l)) (getPlayerCoords x)
       (a1,b1)= nearestCoord (foodLocation m (0,0)) (getPlayerCoords x)


{-| ==Introdução 
Nesta Tarefa, foi-nos proposta a criação de um comportamento para os Fantasmas presentes no labirinto. Para isso, criamos um código que define um comportamento para estes Fantasmas que se baseia numa análise das coordenadas do jogador que pretende seguir juntamente com um movimento que se guia através de colisão com paredes.
--Analisando os resultados, conseguimos ver que a forma de perseguir o Pac-Man, bem como a forma de fugir deste não são ideias pois, apesar de funcionarem, são relativamente fáceis de escapar e apanhar respetivamente.

==Objetivos
Tendo primeiro em conta o comportamento do Fantasma quando está Dying, ou seja, quando tem de fugir do Pac-Man, decidimos implementar um comportamento que se baseia em andar no sentido dos ponteiros do relógio em volta de  uma zona com Walls. 
Para tal, implementamos a função deadGhostMovement que, de acordo com a orientação do Fantasma, faz com que este continue em frente até encontrar uma Wall e, consequentemente obrigá-lo a andar em sentido dos ponteiros do relógio, como antes mencionado.
Para implementarmos esta função na scatterMode, que é a função que define o movimento dos Fantasmas quando têm de fugir, utilizamos algumas funções auxiliares, nomeadamente a função idDetection, que procura o id introduzido na lista de jogadores de um dado State,
identificando qual jogador corresponde a um Fantasma para poder ser aplicada a DeadGhostMovement. Outra auxiliar utilizada é a função ghostPriority, que coloca os Fantasmas na head da lista de players presente num state. 
A junção de ambas estas funções define a scatterMode que , após detetar os ids necessários no state, aplica a movimentação.
Atendendo agora ao movimento do Fantasma quando quer seguir o Pac-Man, decidimos implementar um tipo de movimento que, após calcular as coordenadas do jogador que vai ser perseguido, o Fantasma vai se movimentar para a direita ou para a esquerda, com o intuito 
de igualar o seu eixo y ao do jogador, começando a descer ou a subir quando se encontra neste mesmo eixo y. O nosso maior desafio na implementação deste movimento foi arranjar forma de fazer com que o Fantasma soubesse desviar-se das paredes e, ao mesmo tempo,
escolher a maneira como se irá desviar que faz com que se aproxime cada vez mais do jogador. Para tal, implementamos uma função auxiliar distance que calcula a distância entre duas coordenadas e fizemos com que, assim que o Fantasma colidir com uma parede,
conseguir decidir qual movimento irá realizar para se desviar da parede que irá diminuir ao máximo a sua distância do Pac-Man.

==Discussão e Conclusão
Após analisar os resultados, conseguimos aferir que, tal como referi na introdução, apesar de os movimentos feitos pelo Fantasma foram ao encontro do nosso objetivo, conseguimos reconhecer que não representam uma solução ideal, visto que não é
particularmente difícil fugir destes Fantasmas, da mesma forma que apanhá-los também se torna fácil. Apesar de tudo, creio que os comportamentos que criamos fazem um bom trabalho no que toca a ir de encontro ao jogador pretendido evitando os obstáculos.
-}

-- |Função utilizada para detetar o tipo de player, que devolve true se for um Pac-man e False se for um Ghost
playerDetection:: Player->Bool
playerDetection (Pacman _) = True
playerDetection _ = False


-- |Função utilizada para colocar os ghosts no inicio da lista de players presente num state
ghostPriority::State->State
ghostPriority (State m [] l) = State m [] l
ghostPriority (State m (x:xs) l) | playerDetection x = State m (xs++[x]) l
                                 | otherwise = aux x (ghostPriority (State m xs l))
  where aux x (State m [] l)= State m [x] l
        aux x (State m xs l)= State m (x:xs) l 

getGhostModeFromState:: State->GhostMode
getGhostModeFromState (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s )] l)= s


-- |Função que devolve as coordenadas do pacman presente no state
getPacPlayerCoords:: State->Coords
getPacPlayerCoords (State m [] l) = (0,0)
getPacPlayerCoords (State m (x:xs) l) | playerDetection x = getPlayerCoords x
                                      | otherwise = getPacPlayerCoords (State m xs l)


-- |Função que encontra o ghost com o id igual ao introduzido e aplica a função de movimentação
idDetection:: State->Int -> Play
idDetection (State m (x:xs) l ) i | getPlayerID x == i =  deadGhostMovement (State m [x] l) (getPacPlayerCoords (State m (x:xs) l))
                                  | otherwise = idDetection (State m xs l ) i

-- |Função que deteta se uma certa se uma coordenada tem uma Wall 
wallDetection:: Coords->Maze->Bool
wallDetection (a,b) m = if whichPiece (a,b) m == Wall then True
                                                      else False

-- |Função que define o comportamento do ghost quando este se encontra Dead, andando às voltas encostado a uma parede no sentido dos ponteiros do relogio
deadGhostMovement:: State->Coords ->Play

deadGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,D,p,v) Dead )] l) (x1,y1) | wallDetection (x+1,y) m = Move i L
                                                                                  | otherwise = Move i D

deadGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,U,p,v) Dead )] l) (x1,y1) | wallDetection (x-1,y) m = Move i R
                                                                                  | otherwise= Move i U

deadGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,R,p,v) Dead )] l) (x1,y1) | wallDetection (x,y+1) m = Move i D
                                                                                  | otherwise= Move i R

deadGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,L,p,v) Dead )] l) (x1,y1) | wallDetection (x,y-1) m = Move i U
                                                                                  | otherwise= Move i L

-- |Função aplicada ao ghost quando este se encontra Dead
scatterMode:: State->Int->Play
scatterMode s x = idDetection (ghostPriority s) x


-- |Função alternative para calcular a raiz quadrada
isqrt :: Int -> Int
isqrt i = let d :: Double
              d = fromIntegral i
          in floor (sqrt d)

-- | Função que calcula a distancia entre coordenadas
distance:: Coords->Coords->Int
distance (x1 , y1) (x2 , y2) = isqrt (  x'*x' +  y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2

-- |Função que define o comportamento do ghost quando esta alive
aliveGhostMovement::State->Coords->Play
aliveGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) Alive)] l) (a,b) | y < b && whichPiece (x,y+1) m /= Wall = Move i R
                                                                                 | y < b && whichPiece (x,y+1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                           else Move i D

                                                                                 | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x+1,y) m == Wall = Move i U
                                                                                 | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x-1,y) m == Wall = Move i D
                                                                                 | y < b && whichPiece (x,y+1) m == Wall && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i L                                
                                                                                 | y < b && whichPiece (x,y-1) m /= Wall = Move i L
                                                                                 | y < b && whichPiece (x,y-1) m == Wall = if distance (x+1,y) (a,b) > distance (x-1,y) (a,b) then Move i U
                                                                                                                           else Move i D

                                                                                 | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x+1,y) m == Wall = Move i U
                                                                                 | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x-1,y) m == Wall = Move i D
                                                                                 | y < b && whichPiece (x,y-1) m == Wall && whichPiece (x-1,y) m == Wall && whichPiece (x+1,y) m == Wall = Move i R
                                                                                 | y == b && x < a && whichPiece (x+1,y) m /= Wall = Move i D
                                                                                 | y == b && x < a && whichPiece (x+1,y) m == Wall = if distance (x,y+1) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                    else Move i R

                                                                                 | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y+1) m == Wall = Move i L
                                                                                 | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y-1) m == Wall = Move i R
                                                                                 | y == b && x < a && whichPiece (x+1,y) m == Wall && whichPiece (x,y-1) m == Wall && whichPiece (x,y-1) m == Wall = Move i U
                                                                                 | y == b && x > a && whichPiece (x-1,y) m /= Wall = Move i U
                                                                                 | y == b && x > a && whichPiece (x-1,y) m == Wall = if distance (x,y+1) (a,b) > distance (x,y-1) (a,b) then Move i L
                                                                                                                                     else Move i R

                                                                                 | y == b && x > a && whichPiece (x-1,y) m == Wall && whichPiece (x,y+1) m == Wall = Move i L
                                                                                 | y == b && x > a && whichPiece (x-1,y) m == Wall && whichPiece (x,y+1) m == Wall && whichPiece (x,y-1) m == Wall = Move i D
                                                                                 | otherwise = Move i U

-- |Função que define o comportamento do ghost para seguir o pacman
chaseMode::State->Int->Play
chaseMode (State m (Ghost (GhoState (i,(x,y),ve,o,p,v) Alive):xs) l) id | id==i = aliveGhostMovement (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) Alive)] l) (getPacPlayerCoords (State m (Ghost (GhoState (i,(x,y),ve,o,p,v) Alive):xs) l))
                                                                        | otherwise = chaseMode (State m xs l) id

-- |Função final que aplica a chase mode a scatter mode de acordo com o state e a posiçao do pacman
ghostPlay :: State -> [Play]
ghostPlay (State m [] l )=  []  
ghostPlay (State m (x:xs) l) | playerDetection x = ghostPlay (State m xs l)
                             | otherwise = if getGhostModeFromState (State m [x] l) == Alive then chaseMode (State m [x] l) (getPlayerID x) : ghostPlay (State m xs l)
                                           else scatterMode (State m [x] l) (getPlayerID x) : ghostPlay (State m xs l)
{-| ==Introdução 
Para esta Tarefa, foi-nos proposta a crição de uma função que atualizava o estado do jogo a cada 250 ms (DelayTime)

==Objetivos 
O meu objetivo para esta Tarefa não pode ser alcançado, devido ao facto de que estava a ficar sem tenpo na entrega e, devido a isso, realizei uma Tarefa 4 muito simples e rudimentar, que simplesmente atualiza o estado do jogo sempre que passam os 250 ms,
e mantem este mesmo estado quando não passam mais de 250 ms. A integração da velocidade dos jogadores também não foi possível implementar devido a este facto.

==Discussão e Conclusão
Analisando os resultados, reconheço que esta solução está longe da ideal. Sinto que com um pouco mais tempo seria capaz de a aproximar do resultado pretendido.
-}



defaultDelayTime= 250 --ms

-- |Função que atualiza o State de acordo com os movimentos a cada 250 ms
progress:: State->State 
progress (State m [] l)= State m [] l
progress (State m(x:xs) l)= movement (Move (getPlayerID x) (getPlayerOrientation x)) (progress (State m xs l))

-- |Função que aplica a função progress a cada 250 ms
passTime :: Int -> State -> State
passTime x (State m x1 l) | x< defaultDelayTime= State m x1 l
                          | otherwise= progress (State m x1 l) 


{-| ==Introdução
Nesta Tarefa, foi-nos proposto realizar uma função capaz de compactar as informações presentes no Labirinto em instruções mais concisas, com o intuito de ocuparem menos espaço.

==Objetivos
Tendo em conta que começamos a tarefa com a função que transforma o Labirinto em instruções, a dificuldade surgiu em arranjar alternativas para garantir que a que conseguiamos colocar a mazeToInstruction na função principal compactMaze e, ao mesmo tempo,
garantir que descrevemos o labirinto na sua totalidade no mínimo de instructions possível. Para garantirmos este resultado, aplicamos, por exemplo, a função verticalPattern que, ao ser utilizada, ia minimizar o número de instruções utilizadas.

==Discussão e Conclusão
 Analisando os resultados, conseguimos aferir que foram aplicadas instruções ao labirinto de forma eficiente, mas também conseguimos reconhecer que seria possível reduzir o número de instructions ainda mais.
-}


-- |Função que devolve um corredor no formato Intsruction
corridorToInstruction :: Corridor -> Instruction
corridorToInstruction [] = Instruct []
corridorToInstruction (x:xs) = Instruct (aux 1 x xs)
     where aux n l [] = [(n,l)]
           aux n l (h:ht)
                   | l == h = aux (n+1) h ht
                   | otherwise = (n,l) : aux 1 h ht

-- |Função que devolve um labirinto no formato Instruction
mazeToInstruction :: Maze -> Instructions
mazeToInstruction [] = []
mazeToInstruction l = map corridorToInstruction l 

-- |Função que define as posições das Instructions
positions :: Instructions->Int-> Instruction
positions (x:xs) 0 = x
positions (x:xs) a= positions xs (a-1)

-- |Função que define o padrão vertical das instruções
verticalPattern :: Int -> Instructions -> Instructions
verticalPattern n [] = []
verticalPattern n [x] = [x]
verticalPattern n (y:x:xs)
               | positions (y:x:xs) n == positions (y:x:xs) (n+1) = take (n+1) (y:x:xs) ++ aux n (drop n (y:x:xs))     
               | otherwise = take (n+1) (y:x:xs) ++ aux n (drop n (y:x:xs))
                   where aux n [x,h] = if x==h then [Repeat n] else [h]
                         aux n (x:h:xs) | x==h = Repeat n : aux n (x:xs)
                                        | otherwise = h:    aux n (x:xs)

-- |Função que acumula o numero de vezes que uma peça se repete
accumulator :: Int -> Instructions -> Instructions
accumulator n [] = []
accumulator 0 l = verticalPattern 0 l
accumulator n l | n>=length l-1= l
accumulator n l = case (positions l n) of Instruct x -> accumulator (n+1) (verticalPattern n l)
                                          Repeat x -> accumulator (n+2) (verticalPattern n l)

-- |Função que aplica o acumulador obtido a uma lista de Instruções
general :: Instructions -> Instructions
general [] = accumulator 0 []
general  l  = accumulator 0 l

-- |Função final que compacta o Labirinto todo em instructions
compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze m = general $ mazeToInstruction m
{-| ==Introdução 
 Para esta tarefa, foi-nos propposto realizar as jogadas do Pac-Man com o intuito de possibilitar o movimento deste ao longo do Labirinto. Para implementar estes movimentos, realizamos algumas funções capazes de analisar os conteúdos do labirinto e ,através delas,
 decidir o melhor comportamento do Pac-Man face aos obstáculos e itens que o rodeiam.

==Objetivos 
Para implementarmos os comportamentos do jogador face à posição que irá copiar, foi necessário implementar uma função auxiliar muito importante, sendo esta a whichPiece que, dado um maze e umas coordenadas, indica-nos a peça aí presente.
Através desta função, fomos capazes de implementar todos os casos possíveis nos movimentos do Pac-Man, garantido que este efetua ações diferentes dependendo da peça para onde se irá mover. Por exemplo, se efetuarmos um movimento para a esquerda e o Pac-Man
está encostado a uma parede, simplesmente fazemos com que ele fique no sítio, impossibilitando a movimentação por paredes.
Outras funcionalidades que implementamos são, nomeadamente: garantir que os Fantasmas que são comidos são direcionados para a GhostHouse e garantir que o estado dos Ghosts altera quando o jogador come uma Comida Grande.
 Para integrar todas as funções de movimento na função play, foram necessárias imensas funções de auxílio que atualizam os states de acordo com as jogadas efetuadas e funções que organizam listas de jogadores nos States com o intuito de facilitar a realizção 
de outras funções. O mais complicado nesta Tarefa foi garantir que as funções de movimentos pudessem ser aplicadas a Fantasmas para serem utilizadas nas seguintes Tarefas. Isto deveu-se ao facto de termos de alterar muitas das nossas funções, que por sua vez
causou erros de patterns que tivemos de resolver

==Discussão e Conclusão
 Analisando os resultados, seguro será dizer que o objetivo foi alcançado, visto que conseguimos fazer com que todos os jogadores, sendo estes Pac-Mans ou Ghosts, fossem capazes de se movimentar ao longo do Labirinto. Creio que, depois de efetuar todas as Tarefas, esta seja a mais complicada, devido ao facto de exigir imensa atenção para serem icluídas todas as exceções necessárias.
|-}





loadMaze :: String -> State
loadMaze filepath = unsafePerformIO $ readStateFromFile filepath

readStateFromFile :: String -> IO State
readStateFromFile f = do
                content <- readFile f
                let llines = lines content 
                    (new_map,pl) = convertLinesToPiece llines 0 []
                return (State new_map pl 1)

convertLinesToPiece :: [String] -> Int -> [Player] -> (Maze,[Player])
convertLinesToPiece [] _ l = ([],l)
convertLinesToPiece (x:xs) n l  = let (a,b) = convertLineToPiece x n 0 l 
                                      (a1,b1) = convertLinesToPiece xs (n+1) b
                                  in (a:a1,b1)

convertLineToPiece :: String -> Int -> Int -> [Player] -> ([Piece],[Player])
convertLineToPiece [] _ _ l = ([],l)
convertLineToPiece (z:zs) x y l = let (a,b ) = charToPiece z x y l
                                      (a1,b1) = convertLineToPiece zs x (y+1) b 
                                  in (a:a1,b1)


charToPiece :: Char -> Int -> Int -> [Player] -> (Piece,[Player])
charToPiece c x y l
    | c == '{' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Normal ))   :l) )
    | c == '<' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Closed Normal )) :l) )
    | c == '}' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Open Normal ))   :l) )
    | c == '>' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, L,0,1 ) 0 Closed Normal )) :l) )
    | c == 'V' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Open Normal ))   :l) )
    | c == 'v' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, U,0,1 ) 0 Closed Normal )) :l) )
    | c == '^' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Open Normal ))   :l) )
    | c == '|' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, D,0,1 ) 0 Closed Normal )) :l) )
    | c == 'X' =  (Empty, ((Pacman ( PacState (length l, (x,y), 1, R,0,1 ) 0 Open Dying ))    :l) )
    | c == 'M' =  (Empty, ((Ghost  (  GhoState (length l,(x,y),0,Null,0,1 ) Alive))            :l) )
    | c == '?' =  (Empty, ((Ghost  (  GhoState (length l,(x,y),0,Null,0,1 ) Dead))             :l) )
    | c == 'o' =  (Food Big,l)
    | c == '.' =  (Food Little,l)
    | c == '#' =  (Wall,l)
    | otherwise = (Empty,l)

--Variáves do PlayerState :
-- i= ID
--(x,y)= coordenadas
--ve= velocidade
--o= orientação
--p= pontos
--v= vidas
--c= timer de Mega
--mo= boca do pac
--s= estado do Ghost (Normal, Mega, Dying)

-- |Indica a largura de um corredor
corridorSize::Maze -> Int
corridorSize x = length (head x)

-- |Muda a orientação do jogador 
changeOrientation:: Orientation->Player->Player
changeOrientation a (Pacman ((PacState (i,(x,y),ve,o,p,v)c mo s))) = Pacman (PacState (i,(x,y),ve,a,p,v) c mo s)

-- |Altera o estado dos Fantasmas quando o Pac-man come uma comida Grande
changeGhostStatus:: [Player]->[Player]
changeGhostStatus [] = []
changeGhostStatus [Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)]=    [Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)]
changeGhostStatus (Pacman (PacState (i,(x,y),ve,o,p,v) c mo s):xs)=  Pacman (PacState (i,(x,y),ve,o,p,v) c mo s):changeGhostStatus xs
changeGhostStatus [Ghost (GhoState (i,(x,y),ve,o,p,v) Alive)]=      [Ghost (GhoState (i,(x,y),ve/2,o,p,v) Dead)]
changeGhostStatus [Ghost (GhoState (i,(x,y),ve,o,p,v) Dead)]=       [Ghost (GhoState (i,(x,y),ve,o,p,v) Dead)]
changeGhostStatus (Ghost (GhoState (i,(x,y),ve,o,p,v) Alive):xs)=    Ghost (GhoState (i,(x,y),ve/2,o,p,v) Dead):changeGhostStatus xs
changeGhostStatus (Ghost (GhoState (i,(x,y),ve,o,p,v) Dead):xs)=     Ghost (GhoState (i,(x,y),ve,o,p,v) Dead):changeGhostStatus xs


-- |Indica o comportamento do Pacman quando este se move para a esquerda
moveL :: State -> State
moveL (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l) | y==0 && even (length m)  && x==midHeight m=    State m [Pacman (PacState (i, (x,corridorSize m),ve, o,p,v) c Closed Normal)] l
                                                                         | y==0 && x== midHeight m=                       State m [Pacman (PacState (i, (x,corridorSize m),ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y-1) m == Wall=                  State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y-1) m == Empty       =          State m [Pacman (PacState (i, (x,y-1), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y-1) m == Food Little =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o,p+1,v) c Closed Normal)] l
                                                                         | whichPiece (x,y-1) m == Food Big    =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve ,o,p+5,v) c Closed Mega)] l 
                                                                         | isGhost (whichPiece (x,y-1) m) =if v >= 1 then State m [Pacman (PacState (i, (x,y-1), ve, o,p,v-1) c Closed Normal)] l 
                                                                                                                     else State m [Pacman (PacState (i, (x,y-1), ve, o,p,0)c Closed Dying)] l
                                                                         | otherwise=                                                 State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l

moveL (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l)   | y==0 && even (length m)  && x==midHeight m=    State m [Pacman (PacState (i, (x,corridorSize m),ve, o,p,v) c Open Normal)] l
                                                                             | y==0 && x== midHeight m=                       State m [Pacman (PacState (i, (x,corridorSize m),ve, o,p,v) c Open Normal)] l
                                                                             | whichPiece (x,y-1) m == Wall=                  State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Normal)] l
                                                                             | whichPiece (x,y-1) m == Empty       =          State m [Pacman (PacState (i, (x,y-1), ve, o,p,v) c Open Normal)] l
                                                                             | whichPiece (x,y-1) m == Food Little =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o,p+1,v) c Open Normal)] l
                                                                             | whichPiece (x,y-1) m == Food Big    =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve ,o,p+5,v) c Open Mega)] l 
                                                                             | isGhost (whichPiece (x,y-1) m) =if v >= 1 then State m [Pacman (PacState (i, (x,y-1), ve, o,p,v-1) c Open Normal)] l 
                                                                                                                     else     State m [Pacman (PacState (i, (x,y-1), ve, o,p,0)c Open Dying)] l
                                                                             | otherwise=                                     State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l

-- |Indica o comportamento do Pacman quando este se move para a direita 
moveR :: State -> State
moveR (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l) | y==corridorSize m-1 && even (length m) && x==midHeight m-1= State m [Pacman (PacState (i, (x,0),ve, o,p,v) c Closed Normal)] l
                                                                         | y==corridorSize m-1 && x== midHeight m=                     State m [Pacman (PacState (i, (x,0),ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y+1) m == Wall  =                             State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y+1) m == Empty =                             State m [Pacman (PacState (i,(x,y+1), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x,y+1) m == Food Little =                       State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o,p+1,v) c Closed Normal)] l
                                                                         | whichPiece (x,y+1) m == Food Big =                          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve ,o,p+5,v) c Closed Mega)] l
                                                                         | isGhost (whichPiece (x,y+1) m) =if v >= 1 then              State m [Pacman (PacState (i, (x,y+1), ve, o,p,v-1)c Closed Normal)] l 
                                                                                                                     else              State m [Pacman (PacState (i, (x,y+1), ve, o,p,0)c Closed Dying)] l
                                                                         |otherwise=                                                   State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l

moveR (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l) | y==corridorSize m-1 && even (length m) && x==midHeight m-1= State m [Pacman (PacState (i, (x,0),ve, o,p,v) c Open Normal)] l
                                                                           | y==corridorSize m-1 && x== midHeight m=                     State m [Pacman (PacState (i, (x,0),ve, o,p,v) c Open Normal)] l
                                                                           | whichPiece (x,y+1) m == Wall  =                             State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Normal)] l
                                                                           | whichPiece (x,y+1) m == Empty =                             State m [Pacman (PacState (i,(x,y+1), ve, o,p,v) c Open Normal)] l
                                                                           | whichPiece (x,y+1) m == Food Little =                       State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o,p+1,v) c Open Normal)] l
                                                                           | whichPiece (x,y+1) m == Food Big =                          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve ,o,p+5,v) c Open Mega)] l
                                                                           | isGhost (whichPiece (x,y+1) m) =if v >= 1 then              State m [Pacman (PacState (i, (x,y+1), ve, o,p,v-1)c Open Normal)] l 
                                                                                                                       else              State m [Pacman (PacState (i, (x,y+1), ve, o,p,0)c Open Dying)] l
                                                                         |otherwise=                                                     State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l
-- |Indica o comportamento do Pacman quando este se move para baixo                                                                            
moveD::State -> State
moveD (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l)   | whichPiece (x+1,y) m == Wall =               State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x+1,y) m == Empty=                 State m [Pacman (PacState (i, (x+1,y), ve, o,p,v) c Closed Normal)] l
                                                                         | whichPiece (x+1,y) m == Food Little =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o,p+1,v) c Closed Normal)] l
                                                                         | whichPiece (x+1,y) m == Food Big    =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve ,o,p+5,v) c Closed Mega)] l
                                                                         | isGhost (whichPiece (x+1,y) m) =if v >= 1 then State m [Pacman (PacState (i, (x+1,y), ve, o,p,v-1)c Closed Normal)] l 
                                                                                                                     else State m [Pacman (PacState (i, (x+1,y), ve, o,p,0)c Closed Dying)] l
                                                                         |otherwise=                                                    State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l

moveD (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l) | whichPiece (x+1,y) m == Wall =                 State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Normal)] l
                                                                           | whichPiece (x+1,y) m == Empty=                 State m [Pacman (PacState (i, (x+1,y), ve, o,p,v) c Open Normal)] l
                                                                           | whichPiece (x+1,y) m == Food Little =          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o,p+1,v) c Open Normal)] l
                                                                           | whichPiece (x+1,y) m == Food Big    =          State (replaceElemInMaze (x,y)(Empty) m)[Pacman (PacState (i, (x+1,y), ve ,o,p+5,v) c Open Mega)] l
                                                                           | isGhost (whichPiece (x+1,y) m) =if v >= 1 then State m [Pacman (PacState (i, (x+1,y), ve, o,p,v-1)c Open Normal)] l 
                                                                                                                     else   State m [Pacman (PacState (i, (x+1,y), ve, o,p,0)c Open Dying)] l
                                                                           |otherwise=                                      State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l
-- |Indica o comportamento do Pacman quando este se move para cima
moveU::State -> State
moveU (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l)| whichPiece (x-1,y) m == Wall =                State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Normal)] l
                                                                        | whichPiece (x-1,y) m == Empty=                State m [Pacman (PacState (i, (x-1,y), ve, o,p,v) c Closed Normal)] l
                                                                        | whichPiece (x-1,y) m == Food Little=          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o,p+1,v) c Closed Normal)] l
                                                                        | whichPiece (x-1,y) m == Food Big=             State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve ,o,p+5,v) c Closed Mega)] l
                                                                        | isGhost (whichPiece (x-1,y) m)=if v >= 1 then State m [Pacman (PacState (i, (x-1,y), ve, o,p,v-1)c Closed Normal)] l 
                                                                                                                   else State m [Pacman (PacState (i, (x-1,y), ve, o,p,0)c Closed Dying)] l
                                                                        | otherwise=                                                State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l

moveU (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Normal)] l)| whichPiece (x-1,y) m == Wall =                State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Normal)] l
                                                                          | whichPiece (x-1,y) m == Empty=                State m [Pacman (PacState (i, (x-1,y), ve, o,p,v) c Open Normal)] l
                                                                          | whichPiece (x-1,y) m == Food Little=          State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o,p+1,v) c Open Normal)] l
                                                                          | whichPiece (x-1,y) m == Food Big=             State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve ,o,p+5,v) c Open Mega)] l
                                                                          | isGhost (whichPiece (x-1,y) m)=if v >= 1 then State m [Pacman (PacState (i, (x-1,y), ve, o,p,v-1)c Open Normal)] l 
                                                                                                                 else     State m [Pacman (PacState (i, (x-1,y), ve, o,p,0)c Open Dying)] l
                                                                          | otherwise=                                    State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Normal)] l
-- |Indica o comportamento do Pacman quando este se move para a esquerda e se encotra no estado mega
moveLMega :: State -> State
moveLMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l)| y == 0 && even (length m)  && x == midHeight m= State m [Pacman (PacState (i, (x,corridorSize m),ve,o,p,v) c Closed Mega)] l
                                                                          | y == 0 && x == midHeight m =                    State m [Pacman (PacState (i, (x,corridorSize m),ve,o,p,v) c Closed Mega)] l
                                                                          | whichPiece (x,y-1) m == Wall =                  State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Mega)] l
                                                                          | whichPiece (x,y-1) m == Food Big =              State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o, p+5, v) c Closed Mega)] l  
                                                                          | whichPiece (x,y-1) m == Food Little =           State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o,p+1,v) c Closed Mega)] l
                                                                          | whichPiece (x,y-1) m == Empty =                 State m [Pacman (PacState (i, (x,y-1), ve, o,p,v) c Closed Mega)] l
                                                                          | isGhost (whichPiece (x,y-1) m) =                State m [Pacman (PacState (i, (x,y-1), ve, o,p+10,v) c Closed Mega)] l
                                                                          | otherwise =                                     State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l 

moveLMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l)| y == 0 && even (length m)  && x == midHeight m= State m [Pacman (PacState (i, (x,corridorSize m),ve,o,p,v) c Open Mega)] l
                                                                            | y == 0 && x == midHeight m =                    State m [Pacman (PacState (i, (x,corridorSize m),ve,o,p,v) c Open Mega)] l
                                                                            | whichPiece (x,y-1) m == Wall =                  State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Mega)] l
                                                                            | whichPiece (x,y-1) m == Food Big =              State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o, p+5, v) c Open Mega)] l  
                                                                            | whichPiece (x,y-1) m == Food Little =           State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y-1), ve, o,p+1,v) c Open Mega)] l
                                                                            | whichPiece (x,y-1) m == Empty =                 State m [Pacman (PacState (i, (x,y-1), ve, o,p,v) c Open Mega)] l
                                                                            | isGhost (whichPiece (x,y-1) m) =                State m [Pacman (PacState (i, (x,y-1), ve, o,p+10,v) c Open Mega)] l
                                                                            | otherwise =                                     State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l 
-- |Indica o comportamento do Pacman quando este se move para a direita e se encotra no estado mega
moveRMega :: State -> State
moveRMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l)| y == corridorSize m-1 && even (length m) && x == midHeight m-1 = State m [Pacman (PacState (i, (x,0),ve,o,p,v) c Closed Mega)] l  
                                                                          | y == corridorSize m-1 && x == midHeight m-1 =                    State m [Pacman (PacState (i, (x,0),ve,o,p,v) c Closed Mega)] l 
                                                                          | whichPiece (x,y+1) m == Wall =                                   State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Mega)] l
                                                                          | whichPiece (x,y+1) m == Food Big =                               State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o, p+5, v) c Closed Mega)] l  
                                                                          | whichPiece (x,y+1) m == Food Little =                            State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o, p+1,v) c Closed Mega)] l
                                                                          | whichPiece (x,y+1) m == Empty =                                  State m [Pacman (PacState (i, (x,y+1), ve, o,p,v) c Closed Mega)] l
                                                                          | isGhost (whichPiece (x,y+1) m)  =                                State m [Pacman (PacState (i, (x,y+1), ve, o,p+10,v) c Closed Mega)] l
                                                                          | otherwise =                                                      State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l 

moveRMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l)| y == corridorSize m-1 && even (length m) && x == midHeight m-1 = State m [Pacman (PacState (i, (x,0),ve,o,p,v) c Open Mega)] l  
                                                                            | y == corridorSize m-1 && x == midHeight m-1 =                    State m [Pacman (PacState (i, (x,0),ve,o,p,v) c Open  Mega)] l 
                                                                            | whichPiece (x,y+1) m == Wall =                                   State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Mega)] l
                                                                            | whichPiece (x,y+1) m == Food Big =                               State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o, p+5, v) c Open Mega)] l  
                                                                            | whichPiece (x,y+1) m == Food Little =                            State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x,y+1), ve, o, p+1,v) c Open Mega)] l
                                                                            | whichPiece (x,y+1) m == Empty =                                  State m [Pacman (PacState (i, (x,y+1), ve, o,p,v) c Open Mega)] l
                                                                            |isGhost (whichPiece (x,y+1) m)  =                                 State m [Pacman (PacState (i, (x,y+1), ve, o,p+10,v) c Open Mega)] l
                                                                            | otherwise =                                                      State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l 
-- |Indica o comportamento do Pacman quando este se move para cima e se encotra no estado mega
moveUMega :: State -> State
moveUMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l) | whichPiece (x-1,y) m == Wall =        State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Mega)] l
                                                                           | whichPiece (x-1,y) m == Food Big =    State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o, p+5, v) c Closed Mega)] l  
                                                                           | whichPiece (x-1,y) m == Food Little = State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o,p+1,v) c Closed Mega)] l
                                                                           | whichPiece (x-1,y) m == Empty =       State m [Pacman (PacState (i, (x-1,y), ve, o,p,v) c Closed Mega)] l
                                                                           | isGhost (whichPiece (x-1,y) m) =      State m [Pacman (PacState (i, (x-1,y), ve, o,p+10,v) c Closed Mega)] l
                                                                           | otherwise =                           State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l 

moveUMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l) | whichPiece (x-1,y) m == Wall =        State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Mega)] l
                                                                             | whichPiece (x-1,y) m == Food Big =    State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o, p+5, v) c Open Mega)] l  
                                                                             | whichPiece (x-1,y) m == Food Little = State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x-1,y), ve, o,p+1,v) c Open Mega)] l
                                                                             | whichPiece (x-1,y) m == Empty =       State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Mega)] l
                                                                             | isGhost (whichPiece (x-1,y) m) =      State m [Pacman (PacState (i, (x,y), ve, o,p+10,v) c Open Mega)] l
                                                                             | otherwise =                           State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l 
-- |Indica o comportamento do Pacman quando este se move para baixo e se encotra no estado mega
moveDMega :: State -> State
moveDMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l)| whichPiece (x+1,y) m == Wall =        State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Closed Mega)] l
                                                                          | whichPiece (x+1,y) m == Food Big =    State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o, p+5, v) c Closed Mega)] l  
                                                                          | whichPiece (x+1,y) m == Food Little = State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o,p+1,v) c Closed Mega)] l
                                                                          | whichPiece (x+1,y) m == Empty =       State m [Pacman (PacState (i, (x+1,y), ve, o,p,v) c Closed Mega)] l
                                                                          | isGhost (whichPiece (x+1,y) m)=       State m [Pacman (PacState (i, (x+1,y), ve, o,p+10,v) c Closed Mega)] l
                                                                          | otherwise =                           State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l 

moveDMega (State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Closed Mega)] l)| whichPiece (x+1,y) m == Wall =         State m [Pacman (PacState (i, (x,y), ve, o,p,v) c Open Mega)] l
                                                                            | whichPiece (x+1,y) m == Food Big =     State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o, p+5, v) c Open Mega)] l  
                                                                            | whichPiece (x+1,y) m == Food Little =  State (replaceElemInMaze (x,y)(Empty) m) [Pacman (PacState (i, (x+1,y), ve, o,p+1,v) c Open Mega)] l
                                                                            | whichPiece (x+1,y) m == Empty =        State m [Pacman (PacState (i, (x+1,y), ve, o,p,v) c Open Mega)] l
                                                                            | isGhost (whichPiece (x+1,y) m)=        State m [Pacman (PacState (i, (x+1,y), ve, o,p+10,v) c Open Mega)] l
                                                                            | otherwise =                            State m [Pacman (PacState (i, (x,y),ve, o,p,v) c Open Mega)] l 

-- |Indica a peça que se encontra nas coordenadas introduzidas
whichPiece:: Coords-> Maze -> Piece  
whichPiece (x,y) m = head (drop y (head (drop x m)))

-- |Atualiza um dado State de acordo com as jogadas efetuadas
convertState :: State -> State
convertState (State m (x:xs) l) = State m [x] l

-- |Devolve os players que se encontram num dado State
playerRemove :: State -> [Player]
playerRemove (State maze [] l)= []
playerRemove (State maze (x:xs) l) = x:xs

-- |Função que testa se um certo player é um fantasma ou não
isGhost :: Piece -> Bool 
isGhost  (PacPlayer (Ghost _)) = True
isGhost _ = False 

isPacman::Piece ->Bool
isPacman (PacPlayer (Pacman _))=True
isPacman _= False

-- |Função que decide se o player vai efetuar um movimento para uma dada direção ou se simplesmente altera de orientação
direction:: Play->State->State
direction (Move id d) (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Normal)] l)  |d==R && o==R= moveR (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Normal)] l)
                                                                                     |d==R && o/=R= State m [Pacman (PacState (i,(x,y),ve,R,p,v)c mo Normal)] l
                                                                                     |d==L && o==L= moveL (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Normal)] l)
                                                                                     |d==L && o/=L= State m [Pacman (PacState (i,(x,y),ve,L,p,v)c mo Normal)] l
                                                                                     |d==U && o==U= moveU (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Normal)] l)
                                                                                     |d==U && o/=U= State m [Pacman (PacState (i,(x,y),ve,U,p,v)c mo Normal)] l
                                                                                     |d==D && o==D= moveD (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Normal)] l)
                                                                                     |otherwise=    State m [Pacman (PacState (i,(x,y),ve,D,p,v)c mo Normal)] l

direction (Move id d) (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Mega)] l)    |d==R && o==R= moveRMega (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Mega)] l)
                                                                                     |d==R && o/=R= State m [Pacman (PacState (i,(x,y),ve,R,p,v)c mo Mega)] l
                                                                                     |d==L && o==L= moveLMega (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Mega)] l)
                                                                                     |d==L && o/=L= State m [Pacman (PacState (i,(x,y),ve,L,p,v)c mo Mega)] l
                                                                                     |d==U && o==U= moveUMega (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Mega)] l)
                                                                                     |d==U && o/=U= State m [Pacman (PacState (i,(x,y),ve,U,p,v)c mo Mega)] l
                                                                                     |d==D && o==D= moveDMega (State m [Pacman (PacState (i,(x,y),ve,o,p,v)c mo Mega)] l)
                                                                                     |otherwise=    State m [Pacman (PacState (i,(x,y),ve,D,p,v)c mo Mega)] l

direction (Move id d) (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)            | d == R && o == R = moveRG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)
                                                                                     | d == R && o /= R = State m [Ghost (GhoState (i,(x,y),ve,R,p,v) s)] l
                                                                                     | d == L && o == L = moveLG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)
                                                                                     | d == L && o /= L = State m [Ghost (GhoState (i,(x,y),ve,L,p,v) s)] l
                                                                                     | d == U && o == U = moveUG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)
                                                                                     | d == U && o /= U = State m [Ghost (GhoState (i,(x,y),ve,U,p,v) s)] l
                                                                                     | d == D && o == D = moveDG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)
                                                                                     | otherwise = State m [Ghost (GhoState (i,(x,y),ve,D,p,v) s)] l
-- |Função que deteta qual o player que irá efetuar o movimento introduzido
movement:: Play->State->State
movement (Move id d) (State m [] l)= State m [] l
movement (Move id d) (State m (x:xs)l) | id==getPlayerID x = direction (Move id d) (convertState (State m [x] l))
                                       | otherwise= movement (Move id d) (State m xs l)

-- |Função que devolve um dado jogador presente num state
stateToPlayer:: State->Player
stateToPlayer (State m (Pacman (PacState (i,(x,y),ve,o,p,v) c mo s):xs) l) = Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)
stateToPlayer (State m (Ghost (GhoState (i,(x,y),ve,o,p,v) s):xs) l) = Ghost (GhoState (i,(x,y),ve,o,p,v) s)


-- |Função que envia o fantasma para a casa depois de ser comido
ghostReSpawn :: State -> State
ghostReSpawn (State m [] l) = State m [] l
ghostReSpawn (State m (h:(Ghost (GhoState (i,(x,y),ve,o,p,v) s)):xs) l) | odd (length m) = State m (h:Ghost (GhoState (i,(midHeight m, div (corridorSize m) 2),ve,o,p,v) Alive):xs) l
                                                                        | otherwise = State m (h:Ghost (GhoState (i,(midHeight m-1,div (corridorSize m) 2),ve,o,p,v) Alive):xs) l

colision :: State -> State
colision (State m [] l) = State m [] l
colision (State m [x] l) = State m [x] l
colision (State m (h:x:t) l) | getPlayerCoords h == getPlayerCoords x = ghostReSpawn (State m (h:x:t) l)
                             | otherwise = colisionState x (colision (State m (h:t) l))

colisionState:: Player->State->State
colisionState p (State m [] l )= State m [p] l
colisionState p (State m (x:xs) l)= State m (x:p:xs) l

-- |Função que, dado um movimento introduzido, devolve um state apenas com o player que irá efetuar esse movimento
getState:: Play->State->State
getState (Move id d ) (State m [] l)= State m [] l
getState (Move id d ) (State m (x:xs)l)| getPlayerID x==id = State m [x] l
                                       | otherwise= getState (Move id d ) (State m xs l)

-- |Função que filtra um state que possui players iguais ao que foi introduzido
playerFilter :: Player -> State -> State
playerFilter x (State m (x2:xs) l) = State m (filter (/= x) (x2:xs)) l

-- |Função que adiciona um player a um dado state
addPlayerToState:: State  -> State -> State
addPlayerToState (State m2 [(Pacman (PacState (i,(x,y),ve,o,p,v) c mo s))] l2) (State m [] l)= State m [Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)] l
addPlayerToState (State m2 [(Pacman (PacState (i,(x,y),ve,o,p,v) c mo s))] l2) (State m (x2:xs) l) | getPacmanMode (Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)) == Mega = if getPlayerID (Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)) < getPlayerID x2 then colision (State m2 ((Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)):(changeGhostStatus (x2:xs))) l) 
                                                                                                                                                                           else (State m2 (x2:(stateToPlayer (addPlayerToState (State m2 [(Pacman (PacState (i,(x,y),ve,o,p,v) c mo s))] l2) (State m xs l))):xs) l)
                                                                                                   | otherwise = if getPlayerID (Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)) < getPlayerID x2 then (State m2 ((Pacman (PacState (i,(x,y),ve,o,p,v) c mo s)):x2:xs) l) 
                                                                                                                 else (State m2 (x2:(stateToPlayer (addPlayerToState (State m2 [(Pacman (PacState (i,(x,y),ve,o,p,v) c mo s))] l2) (State m xs l))):xs) l)
addPlayerToState (State m2 ([Ghost (GhoState (i,(x,y),ve,o,p,v) s)]) l2) (State m [] l) = State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l
addPlayerToState (State m2 ([Ghost (GhoState (i,(x,y),ve,o,p,v) s)]) l2) (State m (x2:xs) l) = if getPlayerID (Ghost (GhoState (i,(x,y),ve,o,p,v) s)) < getPlayerID x2 then (pacmanErase (Ghost (GhoState (i,(x,y),ve,o,p,v) s))(State m2 ((Ghost (GhoState (i,(x,y),ve,o,p,v) s)):x2:xs) l))
                                                                                               else (pacmanErase (Ghost (GhoState (i,(x,y),ve,o,p,v) s))(State m2 ((x2:(stateToPlayer (addPlayerToState (State m2 ([Ghost (GhoState (i,(x,y),ve,o,p,v) s)]) l2) (State m xs l))):xs))l))

-- |Função que elimina o Pacman do state depois de ser apanhado por um ghost
pacmanErase:: Player->State->State
pacmanErase (Ghost (GhoState (i,(x,y),ve,o,p,v) s)) (State m [] l)= (State m [] l)
pacmanErase (Ghost (GhoState (i,(x2,y),ve,o,p,v) s)) (State m (x:xs) l)= (State m ((eraseAux (Ghost (GhoState (i,(x2,y),ve,o,p,v) s)) x): playerRemove (pacmanErase (Ghost (GhoState (i,(x2,y),ve,o,p,v) s)) (State m xs l)))l)

-- |Função auxiliar para a função pacmanErase 
eraseAux:: Player->Player->Player
eraseAux (Ghost (GhoState (i,(x,y),ve,o,p,v) s)) (Ghost (GhoState (i2,(x2,y2),ve2,o2,p2,v2) s2))= (Ghost (GhoState (i2,(x2,y2),ve2,o2,p2,v2) s2))
eraseAux (Ghost (GhoState (i,(x,y),ve,o,p,v) s)) (Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2) c mo Normal))|(x,y)== (x2,y2) =if v2>1 then (Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2-1) c mo Normal))
                                                                                                          else (Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2-1) c mo Dying))
                                                                                                          |otherwise = (Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2) c mo Normal))
eraseAux (Ghost (GhoState (i,(x,y),ve,o,p,v) s)) (Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2) c mo s2))=(Pacman (PacState (i2,(x2,y2),ve2,o2,p2,v2) c mo s2))

-- |Função que filtra um state de acordo com a jogada efetuada, garantindo que este state fica atualizado com os movimentos efetuados pelos players
filteredPlay :: Play -> State -> State
filteredPlay (Move id d) (State m [] l) = State m [] l
filteredPlay (Move id d) (State m (x:xs) l) = playerFilter (stateToPlayer (getState (Move id d) (State m (x:xs) l))) (State m (x:xs) l)

-- |Função final que processa a jogada introduzida, movimentando os jogadores ao longo do Labirinto
play :: Play -> State -> State
play (Move id d) (State m [] l) = State m [] l
play (Move id d) (State m (x:xs) l) = addPlayerToState (movement (Move id d) (State m (x:xs) l)) (filteredPlay (Move id d) (State m (x:xs) l))


-- |Jogadores utilizados para testar o funcionamento do jogo
et = (State mez js 0)
mez = (generateMaze 50 20 2)
js = [ghost1,pacman,ghost2,ghost3,ghost4]
pacman = (Pacman (PacState (1,(16,19),1,R,10,2) 0 Open Normal))
ghost1 = (Ghost (GhoState (2,(10,0),1,R,1,1) Alive))
ghost2 = (Ghost (GhoState (3,(15,3),1,R,1,1) Alive))
ghost3 = (Ghost (GhoState (4,(9,5),1,R,1,1) Dead))
ghost4 = (Ghost (GhoState (5,(4,9),1,R,1,1) Dead))



moveLG :: State -> State
moveLG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)   | y == 0 && even (length m)  && x == midHeight m-1  = State m [Ghost (GhoState (i, (x,corridorSize m-1),ve,o,p,v) s)] l
                                                             | y == 0 && x == midHeight m= State m [Ghost (GhoState (i, (x,corridorSize m-1),ve, o,p,v) s)] l
                                                             | whichPiece (x,y-1) m == Wall = State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x,y-1) m == Empty = State m [Ghost (GhoState (i, (x,y-1), ve, o,p,v) s)] l
                                                             | whichPiece (x,y-1) m == Food Little = State m [Ghost (GhoState (i, (x,y-1), ve, o,p,v) s)] l 
                                                             | whichPiece (x,y-1) m == Food Big = State m [Ghost (GhoState (i, (x,y-1), ve ,o,p,v) s)] l
                                                             | isPacman (whichPiece (x,y-1) m) = if s == Dead then ghostReSpawn (State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l) else State m [Ghost (GhoState (i, (x,y-1), ve, o,p,v) s)] l
                                                             | otherwise = State m [Ghost (GhoState (i, (x,y),ve, o,p,v) s)] l


moveRG :: State -> State
moveRG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)   | y == corridorSize m-1 && even (length m) && x == midHeight m-1 = State m [Ghost (GhoState (i, (x,0),ve, o,p,v) s)] l
                                                             | y == corridorSize m-1 && x == midHeight m = State m [Ghost (GhoState (i, (x,0),ve, o,p,v) s)] l
                                                             | whichPiece (x,y+1) m == Wall = State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x,y+1) m == Empty = State m [Ghost (GhoState (i, (x,y+1), ve, o,p,v) s)] l
                                                             | whichPiece (x,y+1) m == Food Little = State m [Ghost (GhoState (i, (x,y+1), ve, o,p,v) s)] l 
                                                             | whichPiece (x,y+1) m == Food Big = State m [Ghost (GhoState (i, (x,y+1), ve ,o,p,v) s)] l
                                                             | isPacman (whichPiece (x,y+1) m) = if s == Dead then ghostReSpawn (State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l) else State m [Ghost (GhoState (i, (x,y+1), ve, o,p,v) s)] l
                                                             | otherwise = State m [Ghost (GhoState (i, (x,y),ve, o,p,v) s)] l

moveUG :: State -> State
moveUG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)   | whichPiece (x-1,y) m == Wall = State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x-1,y) m == Empty = (State m [Ghost (GhoState (i, (x-1,y), ve, o,p,v) s)] l)
                                                             | whichPiece (x-1,y) m == Food Little = State m [Ghost (GhoState (i, (x-1,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x-1,y) m == Food Big = State m [Ghost (GhoState (i, (x-1,y), ve ,o,p,v) s)] l
                                                             | isPacman (whichPiece (x-1,y) m) = if s == Dead then ghostReSpawn (State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l) else State m [Ghost (GhoState (i, (x-1,y), ve, o,p,v) s)] l
                                                             | otherwise = State m [Ghost (GhoState (i, (x,y),ve, o,p,v) s)] l


moveDG :: State -> State
moveDG (State m [Ghost (GhoState (i,(x,y),ve,o,p,v) s)] l)   | whichPiece (x+1,y) m == Wall = State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x+1,y) m == Empty = State m [Ghost (GhoState (i, (x+1,y), ve, o,p,v) s)] l
                                                             | whichPiece (x+1,y) m == Food Little = State m [Ghost (GhoState (i, (x+1,y), ve, o,p,v) s)] l 
                                                             | whichPiece (x+1,y) m == Food Big = State m [Ghost (GhoState (i, (x+1,y), ve ,o,p,v) s)] l
                                                             | isPacman (whichPiece (x+1,y) m) = if s == Dead then ghostReSpawn (State m [Ghost (GhoState (i, (x,y), ve, o,p,v) s)] l) else State m [Ghost (GhoState (i, (x+1,y), ve, o,p,v) s)] l
                                                             | otherwise = State m [Ghost (GhoState (i, (x,y),ve, o,p,v) s)] l








{-| ==Introdução
 Nesta Tarefa, foi-nos proposta a realização de um labirinto válido para um jogo de Pac-Man. Para tal, decidimos fazer o labirinto começando por criar as paredes laterais e consequentemente colocando algumas estruturas, nomeadamente os túneis e a casa dos Fantasmas.

==Objetivos
Como tinha referido antes, começamos a criação do labirinto através da criação das paredes laterais que rodeiam o local onde irão ser feitas as jogadas. Após a criação das paredes, passamos a gerar o interior de forma aletória, misturando a função generateRandoms
e a função convertMaze que, dado uma lista de números inteiros, gera uma lista de peças correspondentes. Em seguida, passamos por construir os tuneis, usando a função sideWallDestroyer que irá colocar um buraco na zona interior do labirinto, que varia dependendo se
este é par ou ímpar. Finalmente, falta criar uma casa onde iram ser gerados os Fantasmas. Para tal, críamos uma função que retira as peças na zona interior do Labirinto que estão a ocupar o local onde iria ser colocada a Ghost House. Assim que este espaço está livre,
colocamos lá a estrutura que foi criada através dos parâmetros sugeridos no enunciado que, tal como as posiçoes dos túneis, varia de acordo com a largura e o comprimento do labirinto.
Inicialmente, o nosso método era muito diferente. Em vez de criarmos um labirinto aleatório e trocar certas zonas pelas estruturas pretendidas, tencionávamos gerar cada linha separadamente, deixando parâmetros aleatórios nas zonas em que não eram necessárias
peças específicas e, à semelhança da tarefa final, forçava certas zonas a possuírem peças específicas com o intuito de construir as estruturas. Apesar disso, este método estava a criar muitos problemas a compilar e por isso optamos pelo método que descrevi.

==Discussão e Conclusão
Ao analisar os resultados, creio que fomos capazes de criar um labirinto válido, que assegura os parâmetros necessários quando este é par ou ímpar na largura e no comprimento. Apesar disso, creio que seria possivel ter realizado esta tarefa utilizando menos
 funções.
 -}





-- |Converte um inteiro numa peça
convertPiece :: Int -> Piece 
convertPiece  x
    | x == 3 = Food Big
    | x >= 0 && x<70 =Food Little 
    | x >= 70 && x <= 99 = Wall

-- |Converte uma lista de inteiros num corredor
convertCorridor:: [Int]->Corridor  
convertCorridor []=[]
convertCorridor l = map convertPiece l

-- |Converte listas de inteiros num Labirinto com paredes em volta
convertMaze:: [[Int]]-> Maze 
convertMaze []= []
convertMaze (h:t)= ([Wall]++ convertCorridor h ++[Wall]): convertMaze t

printCorridor:: Corridor ->String
printCorridor []= "\n"
printCorridor (h:t)= show h ++ printCorridor t

-- |Gera uma lista de n inteiros entre 0 e 99
generateRandoms:: Int->Int-> [Int] 
generateRandoms n seed = let gen= mkStdGen seed
                         in take n $ randomRs (0,99) gen

{- |Divide uma lista em listas com n elementos
exemplo: subList 3 [1,2,3,4,5,6,7,8,9,10]=  [[1,2,3],[4,5,6],[7,8,9],[10]] -}                        
subList:: Int ->[a]->[[a]] 
subList _ []=[]
subList n l= take n l : subList n (drop n l)


{- |Constroi um corredor com apenas Walls em função da largura
exemplo: wallCorridor 7 = [#,#,#,#,#,#,#]-}
wallCorridor:: Int-> Corridor 
wallCorridor 0 = []
wallCorridor l = Wall : wallCorridor (l-1)

-- |Exemplo de Maze para testar funções no Terminal
sampleMaze :: Maze 
sampleMaze = [
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
                [Empty, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Empty],
                [Empty, Food Little, Food Little, Food Little, Food Little, Wall, Food Little, Empty],
                [Empty, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Empty],
                [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            ]

-- |Exemplo de Corridor para testar funçoes no Terminal
sampleCorridor:: Corridor 
sampleCorridor = [Empty, Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Empty]

-- |Devolve metade da altura do Maze (utilizar para indicar o local do tunel)
midHeight:: Maze-> Int 
midHeight m = div (length m ) 2

-- |Devolve o corredor que se encontra no centro do Maze se for impar e devolve dois corredores que estao no centro se for par
midCorridors:: Maze -> [Corridor] 
midCorridors m = if even (length m) then take 2 $ drop (midHeight m-1) m
                 else take 1 $ drop (midHeight m) m 
                
-- |Destroi as Walls laterais dos corredores , ou seja, troca-as por peças Empty
sideWallDestroyer:: [Corridor]->[Corridor] 
sideWallDestroyer []= []
sideWallDestroyer (c:cs)= ([Empty]++(init (tail c))++ [Empty]): sideWallDestroyer cs 

-- |Aplica a funçao sideWallDestroyer aos corredores do meio, criando assim os tuneis
createTunnel:: Maze-> Maze 
createTunnel m = if even (length m) then take  (midHeight m-1) m ++ sideWallDestroyer (midCorridors m) ++ drop (midHeight m +1) m
                 else take (midHeight m) m ++ sideWallDestroyer (midCorridors m) ++ drop (midHeight m +1) m

-- |Gera o interior do labirinto, excluindo as paredes laterais
buildInterior:: Int->Int->Int-> Maze
buildInterior l h seed= convertMaze $ subList l $ generateRandoms (l*h) seed

{- |Replica uma peça n vezes
exemplo: pieceReplication 6 Empty= [ , , , , , ] -}
pieceReplication:: Int->Piece-> [Piece]
pieceReplication n p = take n (repeat p)

{- |Constroi a casa dos fantasmas em função da largura do Maze
exemplo buildGhostHouse (largura par) = [[ , , , , , , , , , ],[ ,#,#,#, , ,#,#,#, ],[ ,#, , , , , , ,#, ],[ ,#,#,#,#,#,#,#,#, ],[ , , , , , , , , , ]] 
exemplo buildGhostHouse (largura impar)= [[ , , , , , , , , , , ],[ ,#,#,#, , , ,#,#,#, ],[ ,#, , , , , , , ,#, ],[ ,#,#,#,#,#,#,#,#,#, ],[ , , , , , , , , , , ]]-}
buildGhostHouse:: Int-> [Corridor] 
buildGhostHouse l | odd l=    [pieceReplication 11 Empty] ++ 
                              [[Empty]++ pieceReplication 3 Wall ++ pieceReplication 3 Empty ++ pieceReplication 3 Wall ++ [Empty]]++
                              [[Empty]++[Wall]++ pieceReplication 7 Empty ++[Wall]++ [Empty]]++
                              [[Empty]++ pieceReplication 9 Wall ++[Empty]]++
                              [pieceReplication 11 Empty]

                  | otherwise= [pieceReplication 10 Empty] ++ 
                               [[Empty]++ pieceReplication 3 Wall ++ pieceReplication 2 Empty++ pieceReplication 3 Wall ++ [Empty]]++
                               [[Empty]++ [Wall]++ pieceReplication 6 Empty ++[Wall]++ [Empty]]++
                               [[Empty]++ pieceReplication 8 Wall++[Empty]]++
                               [pieceReplication 10 Empty]

-- |Devolve a zona do labirinto onde vai ser construida a casa
ghostHousePosition:: Maze-> [Corridor] 
ghostHousePosition m | even (length m)= take 5 (drop(midHeight m-3) m)
                     | otherwise = take 5 (drop (midHeight m-2) m)

-- |Devolve a parte dos corredores centrais que correspondem à largura da casa, quando a largura do Maze é impar
placeGhostHouseOdd::[Corridor]->[Corridor]->[Corridor] 
placeGhostHouseOdd (c:cs) (h:hs)= (take (div (length c-11) 2) c++ h ++ drop (div(length c-11) 2 +11) c) : placeGhostHouseOdd cs hs
placeGhostHouseOdd _ _ = []

-- |Devolve a parte dos corredores centrais que correspondem à largura da casa, quando a largura do Maze é par
placeGhostHouseEven::[Corridor]->[Corridor]->[Corridor] 
placeGhostHouseEven (c:cs) (h:hs)= (take (div (length c-10) 2) c++ h ++ drop (div (length c-10) 2 +10) c) : placeGhostHouseEven cs hs
placeGhostHouseEven _ _ = []

-- |Junta as funções feitas para a casa de fantasmas e junta-as de acordo com as possiveis alternativas que variam de acordo com os parametros largura e comprimento do Maze
ghostHouseFinal:: Maze-> Maze 
ghostHouseFinal m | odd (length m) && odd (length(head m)) = take (midHeight m -2) m ++ placeGhostHouseOdd   (ghostHousePosition m) (buildGhostHouse (length (head m))) ++ drop (midHeight m +3) m
                  | odd (length m) && even (length(head m))= take (midHeight m -2) m ++ placeGhostHouseEven  (ghostHousePosition m) (buildGhostHouse (length (head m))) ++ drop (midHeight m +3) m
                  | even (length m) && odd (length(head m))= take (midHeight m -3) m ++ placeGhostHouseOdd   (ghostHousePosition m) (buildGhostHouse (length (head m))) ++ drop (midHeight m +2) m
                  | otherwise                              = take (midHeight m -3) m ++ placeGhostHouseEven  (ghostHousePosition m) (buildGhostHouse (length (head m))) ++ drop (midHeight m +2) m   
    
-- |Função utilizada para visualizar o Labirinto no terminal, para poderem ser detetados erros
mazeVisualizer:: Int->Int->Int-> IO ()
mazeVisualizer l h seed | l>= 15 && h>=10= putStr $ printMaze $ createTunnel $ ghostHouseFinal $ [wallCorridor l]  ++ buildInterior (l-2)  (h-2)  seed ++ [wallCorridor l]
                        | l<  15 && h>=10= putStr $ printMaze $ createTunnel $ ghostHouseFinal $ [wallCorridor 15] ++ buildInterior (15-2) (h-2)  seed ++ [wallCorridor 15]
                        | l>= 15 && h< 10= putStr $ printMaze $ createTunnel $ ghostHouseFinal $ [wallCorridor l]  ++ buildInterior (l-2)  (10-2) seed ++ [wallCorridor l]
                        | otherwise=       putStr $ printMaze $ createTunnel $ ghostHouseFinal $ [wallCorridor 15] ++ buildInterior (15-2) (10-2) seed ++ [wallCorridor 15]

-- |Função final que gera a lista dos corredores com peças do jogo
generateMaze:: Int->Int->Int->Maze
generateMaze l h seed | l>= 15 && h>=10= createTunnel $ ghostHouseFinal $ [wallCorridor l]  ++ buildInterior (l-2)  (h-2)  seed ++ [wallCorridor l]
                      | l<  15 && h>=10= createTunnel $ ghostHouseFinal $ [wallCorridor 15] ++ buildInterior (15-2) (h-2)  seed ++ [wallCorridor 15]
                      | l>= 15 && h< 10= createTunnel $ ghostHouseFinal $ [wallCorridor l]  ++ buildInterior (l-2)  (10-2) seed ++ [wallCorridor l]
                      | otherwise=       createTunnel $ ghostHouseFinal $ [wallCorridor 15] ++ buildInterior (15-2) (10-2) seed ++ [wallCorridor 15]
