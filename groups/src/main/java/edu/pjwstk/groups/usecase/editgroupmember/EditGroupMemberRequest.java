package edu.pjwstk.groups.usecase.editgroupmember;

import jakarta.validation.constraints.PositiveOrZero;
import lombok.Builder;

import java.io.Serializable;

/**
 * DTO for {@link edu.pjwstk.groups.model.GroupMember}
 */
@Builder
public record EditGroupMemberRequest(

        @PositiveOrZero(message = "Group money must be positive or zero")
        Integer groupMoney,

        @PositiveOrZero(message = "Total earned money must be positive or zero")
        Integer totalEarnedMoney

) implements Serializable {
}