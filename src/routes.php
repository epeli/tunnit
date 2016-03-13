<?php
// Routes



$app->get('/hello', function ($request, $response, $args) {

    $response = $response->withHeader('Content-type', 'application/json');

    $body = $response->getBody();
    $body->write(json_encode(array(
        'projects' => array(
            'muu',
            'eka',
            'toka',
        )
    )));

    return $response;

});


$app->get('/[{name}]', function ($request, $response, $args) {
    // Sample log message
    $this->logger->info("Slim-Skeleton '/' route");

    // Render index view
    return $this->renderer->render($response, 'index.phtml', $args);
});
