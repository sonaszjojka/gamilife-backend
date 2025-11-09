package edu.pjwstk.groups.usecase.getgroups;

import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.enums.GroupTypeEnum;
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

    private final GroupJpaRepository groupRepository;
    private final GroupSpecificationBuilder specificationBuilder;

    @Override
    @Transactional(readOnly = true)
    public Page<GetGroupsResult> executeInternal(GetGroupsCommand cmd) {
        log.debug("Fetching groups with filters: {}", cmd);

        GroupTypeEnum groupType;
        try {
            groupType = GroupTypeEnum.fromId(cmd.type());
        } catch (RuntimeException e) {
            throw new GroupTypeNotFoundException("Group type does not exists!");
        }

        Specification<Group> spec = specificationBuilder.buildSpecification(
                cmd.joinCode(),
                groupType,
                cmd.name()
        );

        Pageable pageable = createPageable(cmd);
        Page<Group> groups = groupRepository.findAll(spec, pageable);

        log.debug("Found {} groups", groups.getTotalElements());

        return groups.map(group -> new GetGroupsResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupsResult.GroupTypeDto(group.getGroupType().getTitle()),
                group.getGroupMembers().size()
        ));
    }

    private Pageable createPageable(GetGroupsCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size(),
                Sort.by(Sort.Direction.ASC, "name")
        );
    }
}