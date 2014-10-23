module class_DoublyLink
    implicit none
    private
    public :: DoublyLink
    public :: new_DoublyLink
    public :: toDoublyLink
    
    !Doubly Linked List���\�������̗v�f
    type :: DoublyLink
        private
        class(*)         ,pointer :: value => null()
        class(DoublyLink),pointer :: prev  => null()
        class(DoublyLink),pointer :: next  => null()
    contains
        procedure,pass :: getValue    !class(*),pointer :: value�̒l���擾
        procedure,pass :: setValue    !class(*),pointer :: value�̒l��ݒ�
        procedure,pass :: getNextLink !class(DoublyLink),pointer :: next�̒l���擾
        procedure,pass :: setNextLink !class(DoublyLink),pointer :: next�̒l��ݒ�
        procedure,pass :: getPrevLink !class(DoublyLink),pointer :: prev�̒l���擾
        procedure,pass :: setPrevLink !class(DoublyLink),pointer :: prev�̒l��ݒ�
        procedure,pass :: print       !class(*),pointer :: value�̒l����ʕ\��
        final          :: destructDoublyLink !��n��
    end type DoublyLink
    
    !�R���X�g���N�^�̖��O��new_DoublyLink�ɐݒ�
    interface new_DoublyLink
        procedure constructDoublyLink
    end interface
    
    !�^���I�L���X�g�֐��̖��O��toDoublyLink�ɐݒ�
    interface toDoublyLink
        procedure castToDoublyLinkPointer
    end interface
    
    contains
    
    !-----------------------------------------------------------------------------------------!
    !�w���p�֐��Q
    !-----------------------------------------------------------------------------------------!
    !�R���X�g���N�^�itype-bound procedure�ł͂Ȃ��j
    function constructDoublyLink(value,prevItem, nextItem) result(constructor)
        implicit none
        class(*)                 ,intent(in) :: value
        class(DoublyLink),pointer,intent(in) :: prevItem
        class(DoublyLink),pointer,intent(in) :: nextItem
        
        class(DoublyLink),pointer :: constructor
        
        !allocate and associate a new member in the list.
        allocate(DoublyLink::constructor)
        call constructor%setPrevLink(prevItem)
        call constructor%setNextLink(nextItem)
        
        !allocate list item depended on argument.
        call constructor%setValue(value)
        
    end function constructDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�f�X�g���N�^(type-bound final procedure)
    subroutine destructDoublyLink(item)
        implicit none
        type(DoublyLink) :: item
        
        !print *,"finalize link item"
        call deleteValue(item)
        
    end subroutine destructDoublyLink
    !-----------------------------------------------------------------------------------------!
    !List�A�C�e��������Link�ȊO�̒l���폜
    !-----------------------------------------------------------------------------------------!
    subroutine deleteValue(this)
        implicit none
        class(DoublyLink),intent(inout) :: this
        if(associated(this%value))then
            
            !print *,associated(this%value)
            deallocate(this%value)
        end if
    end subroutine deleteValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�f�[�^�\���i�����C�����C���������݂̂�\���D
    !���ɈӖ��͂Ȃ��̂ō폜�𐄏�
    subroutine print(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        
        select type(value=>this%value)
            type is (integer)
                print *,value
            type is (character(*))
                print *,value
            type is (real)
                print *,value
            class default
                stop 'printLink : unexepted type for link'
        end select
        
    end subroutine print
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !class(*)�Œ�`����Ă���List�̒l��DoublyLink�^�Ȃ炻�̒l�ւ̃|�C���^��Ԃ�
    !�[���I�ȃL���X�g�Ƃ��Ă̎g�p��z��
    function castToDoublyLinkPointer(value) result(linkItem)
        implicit none
        class(*),target :: value

        class(DoublyLink),pointer :: linkItem
        
        select type(value)
        class is(DoublyLink); linkItem => value
        end select
        
    end function castToDoublyLinkPointer
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !����List�v�f�̃Z�b�^�[�ƃQ�b�^�[
    !-----------------------------------------------------------------------------------------!
    !����List�v�f�ւ̃|�C���^���擾
    function getNextLink(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        
        class(DoublyLink),pointer :: getNextLink
        
        getNextLink => this%next
        
    end function getNextLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !����List�v�f�ւ̃|�C���^��ݒ�
    subroutine setNextLink(this, next)
        implicit none
        class(DoublyLink),intent(inout) :: this
        class(DoublyLink),pointer,intent(in) :: next
        
        this%next => next
    end subroutine setNextLink
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !List Item�̒l�̃Z�b�^�[�ƃQ�b�^�[
    !-----------------------------------------------------------------------------------------!
    !�l�ւ̃|�C���^���擾
    function getValue(this) result(value)
        implicit none
        class(DoublyLink),intent(in) :: this
        class(*),pointer :: value
        
        value => this%value
    end function getValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�l�ւ̃|�C���^��ݒ�
    subroutine setValue(this, value)
        implicit none
        class(DoublyLink),intent(inout) :: this
        class(*)         ,intent(in) :: value
        
        allocate(this%value, source=value)
    end subroutine setValue
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !�O��List�v�f�̃Z�b�^�[�ƃQ�b�^�[
    !-----------------------------------------------------------------------------------------!
    !�O��List�v�f�ւ̃|�C���^���擾
    function getPrevLink(this)
        implicit none
        class(DoublyLink),intent(in) :: this
        class(DoublyLink),pointer    :: getPrevLink
        
        getPrevLink => this%prev
        
    end function getPrevLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !�O��List�v�f�ւ̃|�C���^��ݒ�
    subroutine setPrevLink(this, prev)
        implicit none
        class(DoublyLink),        intent(inout) :: this
        class(DoublyLink),pointer,intent(in   ) :: prev
        
        this%prev => prev
    end subroutine setPrevLink
    !-----------------------------------------------------------------------------------------!
    
end module class_DoublyLink
