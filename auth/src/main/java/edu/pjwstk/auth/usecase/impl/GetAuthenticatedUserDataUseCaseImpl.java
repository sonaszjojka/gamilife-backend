package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.auth.security.UserDetailsImpl;
import edu.pjwstk.auth.usecase.GetAuthenticatedUserDataUseCase;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class GetAuthenticatedUserDataUseCaseImpl implements GetAuthenticatedUserDataUseCase {
    @Override
    public Optional<CurrentUserDto> execute() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Object userDetails = authentication.getPrincipal();

        if (userDetails instanceof UserDetailsImpl user) {
            return Optional.of(new CurrentUserDto(user.getId(), user.getUsername()));
        }

        return Optional.empty();
    }
}
