package pl.gamilife.shared.web.security;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;
import pl.gamilife.shared.web.security.annotation.AllowUnverified;

@Component
public class EmailVerificationInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull Object handler
    ) {
        if (!(handler instanceof HandlerMethod handlerMethod)) {
            return true;
        }

        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            return true;
        }

        // If authenticated and even unverified can access it => it means everyone else can access it too
        boolean endpointAllowsUnverified = handlerMethod.hasMethodAnnotation(AllowUnverified.class)
                || handlerMethod.getBeanType().isAnnotationPresent(AllowUnverified.class);
        if (endpointAllowsUnverified) {
            return true;
        }

        boolean isUserVerified = authentication.getAuthorities()
                .stream()
                .anyMatch(a -> a.getAuthority().equals("ROLE_VERIFIED"));
        if (!isUserVerified) {
            throw new AccessDeniedException("User is not verified");
        }

        return true;
    }
}
