package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.groups.repository.jpa.GroupRepositoryJpa;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import edu.pjwstk.groups.util.GroupSpecificationBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupsUseCaseImpl implements GetGroupsUseCase {

    private final GroupRepositoryJpa groupRepository;
    private final GroupSpecificationBuilder specificationBuilder;

    @Override
    @Transactional(readOnly = true)
    public Page<GroupDto> execute(GroupFilterRequest request) {
        log.debug("Fetching groups with filters: {}", request);

        GroupTypeEnum groupType;
        try {
            groupType = GroupTypeEnum.fromId(request.type());
        }catch (RuntimeException e){
            throw new GroupTypeNotFoundException("Group type does not exists!");
        }

        Specification<Group> spec = specificationBuilder.buildSpecification(
                request.joinCode(),
                groupType,
                request.name()
        );

        Pageable pageable = createPageable(request);

        Page<Group> groups = groupRepository.findAll(spec, pageable);

        log.debug("Found {} groups", groups.getTotalElements());

        return groups.map(group -> new GroupDto(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GroupDto.GroupTypeDto(group.getGroupType().getTitle()),
                group.getGroupMembers().size()
        ));
    }

    private Pageable createPageable(GroupFilterRequest request) {
        return PageRequest.of(
                request.page(),
                request.size(),
                Sort.by(Sort.Direction.ASC, "name")
        );
    }
}