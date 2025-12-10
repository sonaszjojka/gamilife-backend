package pl.gamilife.shared.kernel.architecture;

public interface Command {

    /**
     * @deprecated It will be removed in the near future in favour of Beans Validation.
     */
    @Deprecated(forRemoval = true)
    default void validate() {

    }
}
