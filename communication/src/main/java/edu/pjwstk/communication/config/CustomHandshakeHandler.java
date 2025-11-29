package edu.pjwstk.communication.config;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import lombok.AllArgsConstructor;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.WebSocketHandler;
import org.springframework.web.socket.server.support.DefaultHandshakeHandler;

import java.security.Principal;
import java.util.Map;

@Component
@AllArgsConstructor
public class CustomHandshakeHandler extends DefaultHandshakeHandler {

    private final AuthApi authApi;

    @Override
    protected Principal determineUser(
            ServerHttpRequest request,
            WebSocketHandler wsHandler,
            Map<String, Object> attributes
    ) {
        CurrentUserDto user = authApi.getCurrentUser();
        return new StompPrincipal(user.userId().toString());
    }

    record StompPrincipal(String name) implements Principal {
        @Override
        public String getName() {
            return name;
        }
    }

}
