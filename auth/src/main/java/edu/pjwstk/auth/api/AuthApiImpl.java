package edu.pjwstk.auth.api;

import edu.pjwstk.auth.security.UserDetailsImpl;
import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class AuthApiImpl implements AuthApi {
    @Override
    public Optional<CurrentUserDto> getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        Object userDetails = authentication.getPrincipal();

        if (userDetails instanceof UserDetailsImpl user) {
            return Optional.of(new CurrentUserDto(user.getUserId(), user.getUsername()));
        }

        return Optional.empty();
    }
}
