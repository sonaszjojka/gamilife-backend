package pl.gamilife.app.dto.user;

import java.util.UUID;

public record UserPublicDetailsResponse(
        UUID id,
        String firstName,
        String lastName,
        String username,
        Integer level,
        int money,
        boolean isProfilePublic
) implements UserDetailsResponse {
}
