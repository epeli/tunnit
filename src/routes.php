<?php
// Routes



$app->post('/row/{id}', function ($request, $response, $args) {

    $response = $response->withHeader('Content-type', 'application/json');

    error_log('body '  . print_r($request->getParsedBody(), true));
    $route = $request->getAttribute('route');

    $body = $response->getBody();

    $body->write(json_encode(array(
        'status' => 'ok ' . $route->getArgument('id'),
        'body' => $request->getParsedBody(),
    )));

    return $response;

});


$app->get('/[{name}]', function ($request, $response, $args) {
    // Sample log message
    $this->logger->info("Slim-Skeleton '/' route");

    // Render index view
    return $this->renderer->render($response, 'index.phtml', $args);
});
