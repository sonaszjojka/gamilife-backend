package edu.pjwstk.auth.security;

import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.SecureUserInfoApiDto;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class UserDetailsServiceImpl implements UserDetailsService {

    private final UserApi userApi;

    @Value("${spring.codes.verification-code.email-verification-enabled}")
    private boolean isEmailVerificationEnabled;

    public UserDetailsServiceImpl(UserApi userApi) {
        this.userApi = userApi;
    }

    @Override
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        SecureUserInfoApiDto dto = userApi.getSecureUserDataByEmail(email)
                .orElseThrow(() -> new UsernameNotFoundException("User not found with email: " + email));

        List<GrantedAuthority> authorities = new ArrayList<>();

        if (dto.isEmailVerified() || !isEmailVerificationEnabled) {
            authorities.add(new SimpleGrantedAuthority("ROLE_VERIFIED"));
        } else {
            authorities.add(new SimpleGrantedAuthority("ROLE_UNVERIFIED"));
        }

        return new UserDetailsImpl(dto.userId(), dto.email(), dto.password(), dto.passwordChangeDate(), authorities);
    }
}
