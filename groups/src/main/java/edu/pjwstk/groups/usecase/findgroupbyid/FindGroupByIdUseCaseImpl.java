package edu.pjwstk.groups.usecase.findgroupbyid;

import edu.pjwstk.api.groups.dto.GroupDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.repository.GroupRepository;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class FindGroupByIdUseCaseImpl implements FindGroupByIdUseCase {

    private final GroupRepository groupRepository;
    private final FindGroupByIdMapper findGroupByIdMapper;

    public FindGroupByIdUseCaseImpl(GroupRepository groupRepository, FindGroupByIdMapper findGroupByIdMapper) {
        this.groupRepository = groupRepository;
        this.findGroupByIdMapper = findGroupByIdMapper;
    }

    @Override
    public GroupDto execute(UUID groupId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));
        return findGroupByIdMapper.toResponse(group);
    }
}
