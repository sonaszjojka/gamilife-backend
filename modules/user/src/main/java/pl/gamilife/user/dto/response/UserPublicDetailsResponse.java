package pl.gamilife.user.dto.response;

import pl.gamilife.user.domain.User;

import java.util.UUID;

public record UserPublicDetailsResponse(
        UUID id,
        String firstName,
        String lastName,
        String username,
        int level,
        int money,
        boolean isProfilePublic
) implements UserDetailsResponse {

    public static UserPublicDetailsResponse from(User user) {
        return new UserPublicDetailsResponse(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getUsername(),
                user.getLevel(),
                user.getMoney(),
                user.isProfilePublic()
        );
    }
}
