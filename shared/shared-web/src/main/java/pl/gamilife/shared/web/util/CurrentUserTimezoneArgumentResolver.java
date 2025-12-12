package pl.gamilife.shared.web.util;

import org.springframework.core.MethodParameter;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.support.WebDataBinderFactory;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.method.support.HandlerMethodArgumentResolver;
import org.springframework.web.method.support.ModelAndViewContainer;
import pl.gamilife.shared.web.util.annotation.CurrentUserTimezone;

import java.time.DateTimeException;
import java.time.ZoneId;

@Component
public class CurrentUserTimezoneArgumentResolver implements HandlerMethodArgumentResolver {

    private static final String TIMEZONE_HEADER = "X-Timezone";
    private static final String DEFAULT_TIMEZONE = "Europe/Warsaw";

    @Override
    public boolean supportsParameter(MethodParameter parameter) {
        return parameter.hasParameterAnnotation(CurrentUserTimezone.class)
                && ZoneId.class.isAssignableFrom(parameter.getParameterType());
    }

    @Override
    public Object resolveArgument(
            @NonNull MethodParameter parameter,
            ModelAndViewContainer mavContainer,
            @NonNull NativeWebRequest webRequest,
            WebDataBinderFactory binderFactory
    ) {
        String timezoneHeader = webRequest.getHeader(TIMEZONE_HEADER);

        if (timezoneHeader == null || timezoneHeader.isBlank()) {
            return ZoneId.of(DEFAULT_TIMEZONE);
        }

        try {
            return ZoneId.of(timezoneHeader);
        } catch (DateTimeException e) {
            return ZoneId.of(DEFAULT_TIMEZONE);
        }
    }
}
