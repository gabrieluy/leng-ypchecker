module TypeChecker where

    import AbsPascal
    import PrintPascal
    import ErrM
    import Data.Maybe
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Control.Monad
    
    -- ESTRUCTURAS DE DATOS
    
    
    -- MAIN
    typeCheck :: Program -> Err ()
    typeCheck program = checkProgram program
    
    -- CHECK Program
    checkProgram :: Program -> Err ()
    checkProgram (PBlock name varPart procFuns stms) = do  
        varCtx <- crearVarContext varPart
        return ()
    
    -- map checkstm con la lista de stms
    -- primero insertar las finciones en el contexto y despues checkear los stm dentro de las funciones 
    
    crearVarContext :: VarPart -> Err CxtVar
    crearVarContext (VPart vars) = foldM convertirVar Map.empty vars
    
    convertirVar :: CxtVar -> VarDecl -> Err CxtVar
    convertirVar ctx (VDecl idents ty) = foldM (insertIdsIntoContext ty) ctx idents
    
    insertIdsIntoContext :: Type -> CxtVar -> Ident -> Err CxtVar
    insertIdsIntoContext ty ctx ident = if ( Map.notMember ident ctx ) 
                                            then return (Map.insert ident ty ctx) 
                                        else
                                            fail ("La variable ya esta declarada")
    
    --Contexto Ambiente
    typeChecker :: CxtVar -> CxtFun -> Exp -> Type -> Err ()
    typeChecker cVar cFun e t = do
        t2 <- typeInference cVar cFun e
        if t == t2 then
            return ()
        else
            fail ("Los tipos " ++ (show t) ++ " y " ++ (show t2) ++ " no coinciden ")
    
    type CxtVar = Map Ident Type
    type CxtFun = Map Ident ([(Bool, Type)], Maybe Type)
    
    typeInference :: CxtVar -> CxtFun -> Exp -> Err Type
    typeInference cvar cfun (EEq e1 e2)    = checkCompare cvar cfun e1 e2
    typeInference cvar cfun (EDiff e1 e2)  = checkCompare cvar cfun e1 e2 
    typeInference cvar cfun (ELe e1 e2)    = checkCompare cvar cfun e1 e2 
    typeInference cvar cfun (ELeq e1 e2)   = checkCompare cvar cfun e1 e2
    typeInference cvar cfun (EGeq e1 e2)   = checkCompare cvar cfun e1 e2 
    typeInference cvar cfun (EGe e1 e2)    = checkCompare cvar cfun e1 e2
    typeInference cvar cfun (EPlus e1 e2)  = checkExpAritmetica cvar cfun e1 e2
    typeInference cvar cfun (ESubst e1 e2) = checkExpAritmetica cvar cfun e1 e2
    typeInference cvar cfun (EOr e1 e2)    =  checkExpBooleana cvar cfun e1 e2
    typeInference cvar cfun (EMul e1 e2)   = checkExpAritmetica cvar cfun e1 e2
    typeInference cvar cfun (EDiv e1 e2)   = checkExpAritmetica cvar cfun e1 e2
    typeInference cvar cfun (EAnd e1 e2)   = checkExpBooleana cvar cfun e1 e2
    typeInference cvar cfun (EMod e1 e2)   = checkDivMod cvar cfun e1 e2
    typeInference cvar cfun (EDiv2 e1 e2)  = checkDivMod cvar cfun e1 e2
    typeInference cvar cfun (ECall i es)   = case Map.lookup i cfun of
                                                Just (p:params, Just t) -> do 
                                                        esListParamCompatibles cvar cfun (p:params) es
                                                        return t
                                                Just (_, Nothing) -> fail "Se esta llamando a un procedimiento"
                                                Just ([], Just t) -> fail "La funcion debe contener parametros"
                                                Nothing -> fail "Funcion no declarada"
    typeInference cvar cfun (ECallEmpty i) = case Map.lookup i cfun of
                                                Just ([], Just t) -> return t
                                                Just (_, Nothing) -> fail "Se esta llamando a un procedimiento"
                                                Just (p:params, Just t) -> fail "La funcion no puede contener parametros"
                                                Nothing -> fail "Funcion no declarada"
    typeInference cvar cfun (EStr x)       = return Type_string
    typeInference cvar cfun (EInt x)       = return Type_integer
    typeInference cvar cfun (EReal x)      = return Type_real
    typeInference cvar cfun (EChar x)      = return Type_char
    typeInference cvar cfun (ETrue)        = return Type_bool
    typeInference cvar cfun (EFalse)       = return Type_bool
    typeInference cvar cfun (EIdent i)     = case Map.lookup i cvar of
                                                Just t -> return t
                                                Nothing -> fail "Variable no declarada"
    typeInference cvar cfun (ENot e)       = typeInference cvar cfun e
    typeInference cvar cfun (ENegNum e)    = typeInference cvar cfun e
    typeInference cvar cfun (EPlusNum e)   = typeInference cvar cfun e
    
    
    esListParamCompatibles :: CxtVar -> CxtFun -> [(Bool, Type)] -> [Exp] -> Err ()
    esListParamCompatibles cvar cfun (x:xp) (y:ye) = do
                                        esParamCompatible cvar cfun x y 
                                        esListParamCompatibles cvar cfun xp ye
    
    esParamCompatible :: CxtVar -> CxtFun -> (Bool, Type) -> Exp -> Err ()
    esParamCompatible cvar cfun (ref, t1) e = do 
            t2 <- typeInference cvar cfun e
            if sonTiposCompatibles t1 t2
                then if ref 
                        then esVariable e
                     else return ()
            else fail "Los tipos no son compatibles"
    
    esVariable :: Exp -> Err ()
    esVariable e = case e of
        EIdent ident -> return ()
        _ -> fail "La el parametro por referencia no es una variable"  
    
    checkExpAritmetica :: CxtVar -> CxtFun -> Exp -> Exp -> Err Type
    checkExpAritmetica cvar cfun e1 e2 = do 
        t1 <- typeInference cvar cfun e1
        t2 <- typeInference cvar cfun e2
        if sonTipoNumerico t1 t2
            then return (max t1 t2)
        else fail "Alguno de los tipos no es numerico"
    
    checkDivMod :: CxtVar -> CxtFun -> Exp -> Exp -> Err Type
    checkDivMod cvar cfun e1 e2 = do 
        t1 <- typeInference cvar cfun e1
        t2 <- typeInference cvar cfun e2
        if t1 == Type_integer && t2 == Type_integer
            then return Type_integer
        else fail "Alguno de los tipos no es integer"
    
    checkExpBooleana :: CxtVar -> CxtFun -> Exp -> Exp -> Err Type
    checkExpBooleana cvar cfun e1 e2 = do 
        t1 <- typeInference cvar cfun e1
        t2 <- typeInference cvar cfun e2
        if sonTipoBool t1 t2
            then return Type_bool
        else fail "Alguno de los tipos no es booleano"
    
    checkCompare :: CxtVar -> CxtFun -> Exp -> Exp -> Err Type
    checkCompare cvar cfun e1 e2 = do 
        t1 <- typeInference cvar cfun e1
        t2 <- typeInference cvar cfun e2
        if sonTiposCompatibles t1 t2
            then return Type_bool
        else fail "Los tipos no son comparables"
    
    isTipoNumerico :: Type -> Bool
    isTipoNumerico  t = (t == Type_real || t == Type_integer)
    
    sonTipoNumerico :: Type -> Type -> Bool
    sonTipoNumerico t1 t2 = isTipoNumerico t1 && isTipoNumerico t2
    
    sonTipoBool :: Type -> Type -> Bool
    sonTipoBool t1 t2 = t1 == Type_bool && t2 == Type_bool
    
    sonTipoChar :: Type -> Type -> Bool
    sonTipoChar t1 t2 = t1 == Type_char && t2 == Type_char
    
    sonTipoStr :: Type -> Type -> Bool
    sonTipoStr t1 t2 = t1 == Type_string && t2 == Type_string
    
    sonTipoBoolCharStr :: Type -> Type -> Bool
    sonTipoBoolCharStr t1 t2 = sonTipoBool t1 t2 || sonTipoChar t1 t2 || sonTipoStr t1 t2
    
    sonTiposCompatibles :: Type -> Type -> Bool
    sonTiposCompatibles t1 t2 = sonTipoNumerico t1 t2 || sonTipoBoolCharStr t1 t2
    
    run_test_exp_var :: Err ()
    run_test_exp_var = typeChecker (Map.insert (Ident "x") Type_char Map.empty) Map.empty (EIdent (Ident "x")) Type_char
    
    run_unit_test :: Err ()
    run_unit_test = do
        run_test_exp_var
        --run_test_exp_add
    
    --lookup :: Ord k => -> Map k a -> Maybe a
    --https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
    
    
    -- ./bnfc-2.8-win.exe -m Pascal.cf
    -- ghc TypeChecker.hs
    -- para correr los test 
    -- 1) ghci TypeChecker.hs
    -- 2) run_unit_test
    
    --fold (a -> b -> a) -> a -> [b] -> a
    --fold (+) 0 [2,3,4]
    
    --foldM ctx -> var -> m ctx -> Map.empty -> [b] -> m a