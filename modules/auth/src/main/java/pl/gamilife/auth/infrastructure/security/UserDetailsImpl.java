package pl.gamilife.auth.infrastructure.security;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import pl.gamilife.shared.web.security.AuthenticatedUser;

import java.util.Collection;
import java.util.UUID;

@Getter
@AllArgsConstructor
public class UserDetailsImpl implements UserDetails, AuthenticatedUser {
    private final UUID id;
    private final String username;
    private final Collection<? extends GrantedAuthority> authorities;

    @Override
    public String getPassword() {
        return null;
    }
}
