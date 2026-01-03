package pl.gamilife.communication.config;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jspecify.annotations.NonNull;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketTransportRegistration;
import org.springframework.web.socket.handler.WebSocketHandlerDecorator;
import org.springframework.web.socket.handler.WebSocketHandlerDecoratorFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Configuration
@RequiredArgsConstructor
@EnableWebSocketMessageBroker
public class WebSocketConfig implements WebSocketMessageBrokerConfigurer, ApplicationListener<ContextClosedEvent> {

    private final CustomHandshakeHandler customHandshakeHandler;
    private final Map<String, WebSocketSession> activeSessions = new ConcurrentHashMap<>();

    @Value("${app.frontend-urls.main-url}")
    private String frontendUrl;

    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {
        registry.addEndpoint("/ws")
                .setAllowedOriginPatterns(frontendUrl)
                .setHandshakeHandler(customHandshakeHandler)
                .withSockJS();
    }

    @Override
    public void configureMessageBroker(MessageBrokerRegistry config) {
        config.enableSimpleBroker("/queue");
        config.setUserDestinationPrefix("/user");
    }

    @Override
    public void configureWebSocketTransport(WebSocketTransportRegistration registry) {
        registry.addDecoratorFactory(new WebSocketHandlerDecoratorFactory() {
            @Override
            public @NonNull WebSocketHandlerDecorator decorate(final org.springframework.web.socket.WebSocketHandler handler) {
                return new WebSocketHandlerDecorator(handler) {
                    @Override
                    public void afterConnectionEstablished(final @NonNull WebSocketSession session) throws Exception {
                        activeSessions.put(session.getId(), session);
                        super.afterConnectionEstablished(session);
                    }

                    @Override
                    public void afterConnectionClosed(@NonNull WebSocketSession session, @NonNull CloseStatus closeStatus) throws Exception {
                        activeSessions.remove(session.getId());
                        super.afterConnectionClosed(session, closeStatus);
                    }
                };
            }
        });
    }

    @Override
    public void onApplicationEvent(@NonNull ContextClosedEvent event) {
        log.info("Closing {} active WebSocket sessions due to application shutdown.", activeSessions.size());
        for (WebSocketSession session : activeSessions.values()) {
            try {
                if (session.isOpen()) {
                    session.close(CloseStatus.GOING_AWAY);
                }
            } catch (Exception e) {
                log.warn("Error while closing WebSocket session: {}", session.getId(), e);
            }
        }
        activeSessions.clear();
    }

}
