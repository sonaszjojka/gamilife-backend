package pl.gamilife.auth.infrastructure.security;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.domain.model.projection.SecureUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;

import java.util.ArrayList;
import java.util.List;

@Service
public class UserDetailsServiceImpl implements UserDetailsService {

    private final UserContext userContext;

    @Value("${spring.codes.verification-code.email-verification-enabled}")
    private boolean isEmailVerificationEnabled;

    public UserDetailsServiceImpl(UserContext userContext) {
        this.userContext = userContext;
    }

    @Override
    public UserDetails loadUserByUsername(String email) throws UsernameNotFoundException {
        SecureUserDetails dto = userContext.getSecureUserDataByEmail(email)
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
