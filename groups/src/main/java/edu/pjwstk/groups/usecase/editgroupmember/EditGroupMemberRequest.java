package edu.pjwstk.groups.usecase.editgroupmember;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.PositiveOrZero;
import lombok.Builder;

import java.io.Serializable;
import java.time.Instant;

/**
 * DTO for {@link edu.pjwstk.groups.entity.GroupMember}
 */
@Builder
public record EditGroupMemberRequest(

        @PositiveOrZero(message = "Group money must be positive or zero")
        Integer groupMoney,

        @PositiveOrZero(message = "Total earned money must be positive or zero")
        Integer totalEarnedMoney

) implements Serializable {
}