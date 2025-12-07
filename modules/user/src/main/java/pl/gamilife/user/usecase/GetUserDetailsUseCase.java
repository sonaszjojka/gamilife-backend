package pl.gamilife.user.usecase;

import edu.pjwstk.user.dto.response.UserDetailsResponse;
import edu.pjwstk.user.dto.service.UserDetailsDto;
import pl.gamilife.user.dto.service.UserDetailsDto;

import java.util.UUID;

public interface GetUserDetailsUseCase {
    UserDetailsResponse execute(String requesterEmail, UUID targetUserId);
}
