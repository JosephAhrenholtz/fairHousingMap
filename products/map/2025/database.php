<!-- currently not in use; may use if hosting data on Postgres -->

<?php
    $strQry="SELECT *, ST_AsGeoJSON(wkb_geometry, 5) as geom FROM final_2024";

    $db = new PDO('pgsql:host=localhost;port=5432;dbname=sdb_tcac_2024;', 'postgres', '12345');
    $sql = $db->query($strQry);
    $features=[];
    while ($row = $sql->fetch(PDO::FETCH_ASSOC)) {
        $feature=['type'=>'Feature'];
        $feature['geometry']=json_decode($row['geom']);
        unset($row['geom']);
        $feature['properties']=$row;
        array_push($features, $feature);
    }
    $featureCollection=['type'=>'FeatureCollection', 'features'=>$features];
    echo json_encode($featureCollection);
?>