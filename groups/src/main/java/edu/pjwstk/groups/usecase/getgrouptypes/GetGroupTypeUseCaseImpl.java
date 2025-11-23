package edu.pjwstk.groups.usecase.getgrouptypes;

import edu.pjwstk.groups.repository.GroupTypeJpaRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class GetGroupTypeUseCaseImpl implements GetGroupTypesUseCase {

    private final GroupTypeJpaRepository groupTypeRepository;

    @Override
    public List<GetGroupTypesResult> executeInternal(GetGroupTypesCommand command) {
        return groupTypeRepository.findAll().stream().map(groupType -> new GetGroupTypesResult(
                groupType.getGroupTypeId(),
                groupType.getTitle()
        )).toList();
    }
}
