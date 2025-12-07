package pl.gamilife.user.dto.response;

import java.util.Collection;

public record GetUsersResult(
        long totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<UserFullDetailsResponse> content
) {}
        Collection<UserDetailsResponse> content
) {
}
