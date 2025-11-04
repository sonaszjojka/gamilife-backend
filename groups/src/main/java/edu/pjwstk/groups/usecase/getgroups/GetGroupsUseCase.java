package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.groups.shared.GroupTypeEnum;
import org.springframework.data.domain.Page;

public interface GetGroupsUseCase {
    Page<GroupDto> execute(GroupFilterRequest request);
}
