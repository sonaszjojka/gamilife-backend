package edu.pjwstk.auth.security;

import org.springframework.security.core.Authentication;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component("userSecurity")
public class UserSecurity {

    public boolean matchesTokenUserId(Authentication authentication, UUID userId) {
        if (authentication == null || !authentication.isAuthenticated() || authentication.getPrincipal() == null) {
            return false;
        }

        if (authentication.getPrincipal() instanceof UserDetailsImpl userDetails) {
            return userId.equals(userDetails.getId());
        }

        return false;
    }
}
