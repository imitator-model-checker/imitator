# Quelques idées à réaliser

## Sortir les variables discrètes rationelles de PPL

Actuellement, seule les rationelles discrète sont encore utilisées au sein de PPL. Maintenant que nous traitons les mises à jour de manière séquentielle et que nous faisons une ré-écriture des expressions mettant à jour les horloges à l'execution, il n'est plus nécessaire d'avoir les rationelles discrètes comme faisant partie intégrante de PPL. Car nous pouvons écrire des mises à jour d'horloge telle que celle-ci : `y := x * i * i`.

 - Réduction des dimensions dans PPL

## Ne plus supprimer les variables discrètes non utilisées

Une fois que toutes les variables rationelles discrètes seront sorties de PPL, ne plus faire de suppression de variables discrètes et ne mettre que des warnings précisant qu'elle ne sont pas utilisées. La logique devient c'est assez compliquée dès qu'on commence à supprimer : il faut retrouver avec quoi elles sont liées, parfois supprimer des instructions complètes, etc...

 - Simplification du code (l'arbre de dépendance est compliqué)
 - Evite certains bugs qui se produisent quand on cherche une variable qui a été exclue d'une liste...

## Guard et invariant qui autorise des expressions plus complexes

Par exemple, autoriser les syntaxes `if-then-else`, les appels de fonctions, les déclarations de variable locales. C'est à dire autorisé des `bloc de code` comme dans les updates et les fonctions. La seule différence étant que ces blocs de code doivent être impérativement sans effets de bord. 

 - Permet une plus grande expressivité dans les guardes et invariant
 - C'est peut-être déjà intégré, en fait !! (il suffit de réutiliser le type `seq_code_bloc`, mais il faut vérifier qu'il n'y ai aucun effet de bord)

## Init qui autorise des expressions plus complexes

De la même manière que les gardes et invariant, réutiliser le type `seq_code_bloc` dans la partie init.

 - Simplifie le code
 - Plus grande expressivité des inits
 - Permet les appels de fonction, instructions, if-then-else, ...
 - Une seule section pour traiter les discretes et continues
 - Execution séquentielle
 - Permet de ne plus séparer les expressions continues et discrètes !

## Ajouter les expressions ternaires

Ajouter les expressions ternaires : autoriser les if-then-else dans les expressions (booléennes ou non !) ; changer de syntaxe ? (à la … ? … : de Uppaal ?)

Exemple : `when x <= x + i*i + (if b > then 32 else z) + f(z, x)`

 - Je pense qu'il faut une syntaxe différente du if-then-else (car le parser pourrait se marcher dessus)

## Refactoriser le module PTA2Jani avec JsonFormatter

Utiliser le nouveau module `JsonFormatter` pour refactoriser PTA2Jani.

## Syntaxe retirer le mot clé begin pour les fonction

Si faisable, peut-être que ce serait mieux d'avoir la syntaxe : 

```
function f() : void
  var i : int = 0; 
  ...
end
```

plutôt que 

```
function f() : void
begin
  var i : int = 0; 
  ...
end
```