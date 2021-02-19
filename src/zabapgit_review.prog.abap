REPORT zabapgit_review.

PARAMETERS p_trkorr TYPE e070-trkorr OBLIGATORY.

START-OF-SELECTION.
  NEW zcl_abapgit_review( )->release( p_trkorr ).
