u s e s    
     W i n a p i . W i n d o w s ,  
     S y s t e m . S y s U t i l s ,  
     S y s t e m . C l a s s e s ,  
     V c l . F o r m s ,  
     V c l . S t d C t r l s ,  
     V c l . E x t C t r l s ,  
     V c l . C o n t r o l s ,  
     c T r a n s l a t e ,  
     S y s t e m . T y p e s ,  
     S y s t e m . U I T y p e s ,  
     S y s t e m . V a r i a n t s ,  
     F M X . T y p e s ,  
     F M X . C o n t r o l s ,  
     F M X . F o r m s ,  
     F M X . L a y o u t s ,  
     F M X . S t d C t r l s ; ;  
  
 T Y P E  
     T f r m L a n g u a g e   =   c l a s s ( T F o r m )  
         b t n A p p l y L a n g :   T B u t t o n ;  
         b t n R e f r e s h     :   T B u t t o n ;  
         b t n T r a n s l a t e :   T B u t t o n ;  
         g r p C h o o s e       :   T G r o u p B o x ;  
         l b l A u t h o r s     :   T L a b e l ;  
         l b l H i n t           :   T L a b e l ;  
         L i s t B o x           :   T L i s t B o x ;  
         P a n e l 2             :   T P a n e l ;  
         p r o c e d u r e   F o r m D e s t r o y             ( S e n d e r :   T O b j e c t ) ;  
         p r o c e d u r e   L i s t B o x D b l C l i c k     ( S e n d e r :   T O b j e c t ) ;  
         p r o c e d u r e   b t n A p p l y L a n g C l i c k ( S e n d e r :   T O b j e c t ) ;  
         p r o c e d u r e   b t n R e f r e s h C l i c k     ( S e n d e r :   T O b j e c t ) ;  
         p r o c e d u r e   b t n T r a n s l a t e C l i c k ( S e n d e r :   T O b j e c t ) ;  
         p r o c e d u r e   F o r m K e y P r e s s           ( S e n d e r :   T O b j e c t ;   v a r   K e y :   C h a r ) ;  
         p r o c e d u r e   F o r m C l o s e ( S e n d e r :   T O b j e c t ;   v a r   A c t i o n :   T C l o s e A c t i o n ) ;  
     p r i v a t e  
         f u n c t i o n   G e t S e l e c t e d F i l e N a m e :   s t r i n g ;  
         f u n c t i o n   G e t S e l e c t e d F i l e P a t h :   s t r i n g ;  
         p r o c e d u r e   A p p l y L a n g u a g e ;  
     p u b l i c  
         f u n c t i o n   I s E n g l i s h :   B o o l e a n ;     / / u n u s e d  
         f u n c t i o n   P o p u l a t e L a n g u a g e F i l e s :   B o o l e a n ;  
     e n d ;  
   {  
 V A R  
       f r m L a n g u a g e :   T f r m L a n g u a g e ;  
         }  
  
 p r o c e d u r e   S h o w S e l e c t L a n g u a g e ;  
  
  
 I M P L E M E N T A T I O N   { $ R   * . F M X }  
 U S E S  
     c b A p p D a t a ,   c b I N I F i l e ,   c b D i a l o g s ,   c c I O ,   c c T e x t F i l e ,   c m I O ,   F o r m T r a n s l a t o r ;  
  
  
  
 p r o c e d u r e   S h o w S e l e c t L a n g u a g e ;  
 V A R   f r m L a n g u a g e :   T f r m L a n g u a g e ;  
 b e g i n  
   A s s e r t ( T r a n s l a t o r   < >   N I L ) ;  
   F o r c e D i r e c t o r i e s ( T r a n s l a t o r . G e t L a n g F o l d e r ) ;       {   M a k e   s u r e   t h a t   t h e   f o l d e r s   e x i s t s   }  
  
   A p p d a t a . C r e a t e F o r m H i d d e n ( T f r m L a n g u a g e ,   f r m L a n g u a g e ) ;  
   T R Y  
       f r m L a n g u a g e . b t n T r a n s l a t e . V i s i b l e : =   N O T   A p p d a t a . R u n n i n g F i r s t T i m e ;  
       f r m L a n g u a g e . b t n R e f r e s h . V i s i b l e : =   f r m L a n g u a g e . b t n T r a n s l a t e . V i s i b l e ;  
       f r m L a n g u a g e . P o p u l a t e L a n g u a g e F i l e s ;         {   P o p u l a t e   t h e   L i s t B o x   w i t h   l a n g u a g e s   w e   f o u n d   i n   ' L a n g '   f o l d e r   }  
       f r m L a n g u a g e . S h o w M o d a l ;  
   F I N A L L Y  
       F r e e A n d N i l ( f r m L a n g u a g e ) ;  
   E N D ;  
 e n d ;  
  
  
 p r o c e d u r e   T f r m L a n g u a g e . F o r m C l o s e ( S e n d e r :   T O b j e c t ;   v a r   A c t i o n :   T C l o s e A c t i o n ) ;  
 b e g i n  
     A c t i o n : =   T C l o s e A c t i o n . c a F r e e ;  
 e n d ;  
  
 p r o c e d u r e   T f r m L a n g u a g e . F o r m D e s t r o y ( S e n d e r :   T O b j e c t ) ;  
 b e g i n  
     S a v e F o r m B a s e ( S e l f ) ;     / / L o c a l i z a t i o n   w a r n i n g :   D o n ' t   a d d   d e p e n d e n c i e s   t o   C u b i c V i s u a l C o n t r o l s   h e r e !  
 e n d ;  
  
  
 p r o c e d u r e   T f r m L a n g u a g e . F o r m K e y P r e s s ( S e n d e r :   T O b j e c t ;   v a r   K e y :   C h a r ) ;  
 b e g i n  
   i f   O r d ( K e y )   =   V K _ E S C A P E   t h e n   C l o s e ;  
 e n d ;  
  
  
  
  
  
  
  
  
  
  
 { - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
       L O A D   E X I S T I N G   L A N G U A G E   F I L E  
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }  
 p r o c e d u r e   T f r m L a n g u a g e . L i s t B o x D b l C l i c k ( S e n d e r :   T O b j e c t ) ;  
 b e g i n  
   A p p l y L a n g u a g e ;  
 e n d ;  
  
  
 p r o c e d u r e   T f r m L a n g u a g e . A p p l y L a n g u a g e ;  
 b e g i n  
     i f   L i s t B o x . I t e m I n d e x   =   0   t h e n   E X I T ;  
  
     i f   G e t S e l e c t e d F i l e N a m e   =   ' '  
     t h e n   M e s a j I n f o ( ' P l e 