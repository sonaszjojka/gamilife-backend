package pl.gamilife.auth.infrastructure.security;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import org.springframework.lang.NonNull;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import pl.gamilife.auth.application.dto.TokenClaims;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.shared.web.security.TokenAuthenticationFilter;

import java.io.IOException;
import java.util.List;

@AllArgsConstructor
public class JwtAuthenticationFilter extends TokenAuthenticationFilter {

    private final TokenService tokenService;

    @Override
    protected void doFilterInternal(@NonNull HttpServletRequest request, @NonNull HttpServletResponse response, @NonNull FilterChain filterChain)
            throws ServletException, IOException {
        String token = extractToken(request);
        if (token != null) {
            try {
                TokenClaims claims = tokenService.validateTokenAndExtractClaims(token);
                Authentication authentication = retrieveAuthentication(claims);
                SecurityContextHolder.getContext().setAuthentication(authentication);
            } catch (UsernameNotFoundException | BadCredentialsException e) {
                SecurityContextHolder.getContext().setAuthentication(null);
            }
        }
        filterChain.doFilter(request, response);
    }

    private Authentication retrieveAuthentication(TokenClaims claims) {
        UserDetailsImpl userDetails = new UserDetailsImpl(
                claims.userId(),
                claims.email(),
                List.of(new SimpleGrantedAuthority(claims.isEmailVerified()
                        ? "ROLE_VERIFIED"
                        : "ROLE_UNVERIFIED"
                ))
        );

        return new UsernamePasswordAuthenticationToken(
                userDetails,
                null,
                userDetails.getAuthorities()
        );
    }

    private String extractToken(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();

        if (cookies != null) {
            for (Cookie cookie : cookies) {
                if ("ACCESS-TOKEN".equals(cookie.getName())) {
                    return cookie.getValue();
                }
            }
        }

        return null;
    }
}