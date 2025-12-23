package pl.gamilife.user.usecase.getusers;

import java.util.UUID;

public record GetUsersResult(
        UUID userId,
        String username,
        Integer level,
        boolean isProfilePublic
) {
}
