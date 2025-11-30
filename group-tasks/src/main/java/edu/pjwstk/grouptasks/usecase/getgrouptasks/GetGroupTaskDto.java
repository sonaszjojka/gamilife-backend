package edu.pjwstk.grouptasks.usecase.getgrouptasks;

import edu.pjwstk.api.tasks.dto.TaskDto;
import lombok.Builder;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

@Builder
public record GetGroupTaskDto(

        UUID groupTaskId,
        Integer reward,
        Instant acceptedDate,
        TaskDto taskDto,
        List<GetGroupTaskMemberDto> groupTaskMembers

) {
}
