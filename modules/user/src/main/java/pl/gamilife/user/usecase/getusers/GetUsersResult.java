package pl.gamilife.user.usecase.getusers;

import java.util.UUID;

public record GetUsersResult(
        UUID id,
        String username,
        Integer level,
        boolean isProfilePublic,
        String email
) {
}
