package pl.gamilife.app.dto.user;

import java.util.UUID;

public record UserPrivateDetailsResponse(
        UUID id,
        String username,
        boolean isProfilePublic
) implements UserDetailsResponse {
}
