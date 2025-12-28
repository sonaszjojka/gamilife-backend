package pl.gamilife.shared.web.security.annotation;

import java.lang.annotation.*;

/**
 * This annotation is used to mark endpoint which should allow unverified users
 * (users without VERIFIED role) to access them.
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface AllowUnverified {
}
