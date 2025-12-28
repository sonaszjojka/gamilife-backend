package pl.gamilife.shared.web.util;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.AllArgsConstructor;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import pl.gamilife.shared.kernel.event.TimezoneDetectedEvent;
import pl.gamilife.shared.web.security.AuthenticatedUser;

import java.time.DateTimeException;
import java.time.ZoneId;

@Component
@Slf4j
@AllArgsConstructor
public class TimezoneInterceptor implements HandlerInterceptor {

    private static final String TIMEZONE_HEADER = "X-Timezone";

    private final ApplicationEventPublisher eventPublisher;

    @Override
    public boolean preHandle(
            @NonNull HttpServletRequest request,
            @NonNull HttpServletResponse response,
            @NonNull Object handler
    ) {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null || !authentication.isAuthenticated() || authentication instanceof AnonymousAuthenticationToken) {
            log.info("No authentication found. Skipping timezone detection.");
            return true;
        }

        Object principal = authentication.getPrincipal();
        if (principal instanceof AuthenticatedUser authenticatedUser) {
            String timezoneHeader = request.getHeader(TIMEZONE_HEADER);

            if (timezoneHeader == null || timezoneHeader.isBlank()) {
                log.info("No timezone header found. Skipping timezone detection.");
                return true;
            }

            try {
                ZoneId.of(timezoneHeader);
                log.info("Timezone header found: {}", timezoneHeader);
                eventPublisher.publishEvent(new TimezoneDetectedEvent(authenticatedUser.getId(), timezoneHeader));
            } catch (DateTimeException ignored) {
                log.warn("Timezone header invalid. Skipping timezone detection.");
                // If timezone incorrect and will be necessary during further request processing
                // It will be up to the use case to decide what to do with it
            }
        }

        return true;
    }
}
