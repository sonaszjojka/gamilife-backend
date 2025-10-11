package edu.pjwstk.auth.security;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;
import java.util.UUID;

public class UserDetailsImpl implements UserDetails {

    private final BasicUserInfoApiDto user;

    public UserDetailsImpl(BasicUserInfoApiDto user) {
        this.user = user;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return List.of();
    }

    public UUID getUserId() {
        return user.userId();
    }

    @Override
    public String getPassword() {
        return user.password();
    }

    @Override
    public String getUsername() {
        return user.email();
    }
}
