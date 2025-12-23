package pl.gamilife.auth.application.usecase.getauthuser;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.auth.infrastructure.security.UserDetailsImpl;

@Service
public class GetAuthenticatedUserDataUseCaseImpl implements GetAuthenticatedUserDataUseCase {
    @Override
    public CurrentUserDto execute(GetAuthenticatedUserCommand cmd) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Object userDetails = authentication.getPrincipal();

        if (userDetails instanceof UserDetailsImpl user) {
            return new CurrentUserDto(user.getId(), user.getUsername());
        }

        throw new AccessDeniedException("User is not authenticated");
    }
}
