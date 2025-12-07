package pl.gamilife.user.usecase;

import pl.gamilife.user.dto.response.UserDetailsResponse;

import java.util.UUID;

public interface GetUserDetailsUseCase {
    UserDetailsResponse execute(String requesterEmail, UUID targetUserId);
}
