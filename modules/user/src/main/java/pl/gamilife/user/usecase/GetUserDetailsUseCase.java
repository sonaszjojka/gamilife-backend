package pl.gamilife.user.usecase;

import pl.gamilife.user.dto.service.UserDetails;

import java.util.UUID;

public interface GetUserDetailsUseCase {
    UserDetails execute(UUID userId);
}
