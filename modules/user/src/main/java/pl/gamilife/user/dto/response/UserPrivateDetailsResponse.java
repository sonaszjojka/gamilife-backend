package edu.pjwstk.user.dto.response;

import edu.pjwstk.user.domain.User;

import java.util.UUID;

public record UserPrivateDetailsResponse(
        UUID id,
        String username,
        boolean isProfilePublic
) implements UserDetailsResponse {

    public static UserPrivateDetailsResponse from(User user) {
        return new UserPrivateDetailsResponse(
                user.getId(),
                user.getUsername(),
                user.isProfilePublic()
        );
    }
}
