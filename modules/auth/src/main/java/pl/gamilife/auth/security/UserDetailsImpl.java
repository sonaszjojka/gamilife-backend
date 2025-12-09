package pl.gamilife.auth.security;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import pl.gamilife.shared.web.security.AuthenticatedUser;

import java.time.Instant;
import java.util.Collection;
import java.util.UUID;

@Getter
@AllArgsConstructor
public class UserDetailsImpl implements UserDetails, AuthenticatedUser {
    private final UUID id;
    private final String username;
    private final String password;
    private final Instant passwordChangeDate;
    private final Collection<? extends GrantedAuthority> authorities;
}
