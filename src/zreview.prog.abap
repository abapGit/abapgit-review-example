REPORT zreview.

PARAMETERS p_trkorr TYPE trkorr OBLIGATORY.

START-OF-SELECTION.
  NEW zcl_review( )->release( p_trkorr ).
