package pl.gamilife.grouptask.usecase.getgrouptasks;

import lombok.Builder;

import java.util.List;

@Builder
public record GetGroupTasksResponse(
        List<GetGroupTaskDto> response
) {
}
