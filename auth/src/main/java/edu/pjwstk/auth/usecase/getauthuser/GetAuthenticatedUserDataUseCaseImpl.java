package edu.pjwstk.auth.usecase.getauthuser;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.auth.security.UserDetailsImpl;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.nio.file.AccessDeniedException;
import java.util.Optional;

@Service
public class GetAuthenticatedUserDataUseCaseImpl implements GetAuthenticatedUserDataUseCase {
    @Override
    public CurrentUserDto executeInternal(GetAuthenticatedUserCommand cmd) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Object userDetails = authentication.getPrincipal();

        if (userDetails instanceof UserDetailsImpl user) {
            return new CurrentUserDto(user.getId(), user.getUsername());
        }

        throw new RuntimeException("Access denied"); // TODO: change to other exception
    }
}
