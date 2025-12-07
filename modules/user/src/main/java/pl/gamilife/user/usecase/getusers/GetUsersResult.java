package pl.gamilife.user.usecase.getusers;

import pl.gamilife.user.dto.response.UserFullDetailsResponse;

import java.util.Collection;

public record GetUsersResult(
        long totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<UserFullDetailsResponse> content
) {
}
